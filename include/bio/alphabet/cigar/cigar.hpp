// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \brief Provides the bio::cigar alphabet.
 * \author Hannes Hauswedell <hannes.hauswedell AT decode.is>
 */

#pragma once

#include <charconv>

#include <bio/alphabet/adaptation/uint.hpp>
#include <bio/alphabet/cigar/cigar_op.hpp>
#include <bio/alphabet/composite/alphabet_tuple_base.hpp>
#include <bio/ranges/container/small_string.hpp>

// ------------------------------------------------------------------
// cigar
// ------------------------------------------------------------------

namespace bio
{

/*!\brief The cigar semialphabet pairs a counter with a bio::cigar_op letter.
 * \ingroup cigar
 * \implements bio::writable_semialphabet
 * \implements bio::trivially_copyable
 * \implements bio::standard_layout
 * \implements std::regular
 *
 * \details
 *
 * This semialphabet represents a unit in a CIGAR string, typically found in the
 * SAM and BAM formats. It consists of a number and a bio::cigar_op symbol.
 *
 * It has a "visual representation", but since this is a string and not a char,
 * the type only models bio::writable_semialphabet and not
 * bio::writable_alphabet.
 * Members for reading/writing the string are provided.
 *
 * To avoid confusion between string and char literal, this alphabet has
 * no user defined literal operators. Always assign from a pair of
 * uint32_t and bio::cigar_op.
 *
 * ### Example
 *
 * \include test/snippet/alphabet/cigar/cigar.cpp
 */
class cigar : public alphabet_tuple_base<cigar, uint32_t, cigar_op>
{
private:
    //!\brief The base class.
    using base_t = alphabet_tuple_base<cigar, uint32_t, cigar_op>;

    //!\cond \brief Befriend bio::alphabet_tuple_base.
    friend base_t;
    //!\endcond

public:
    /*!\name Constructors, destructor and assignment
     * \{
     */
    constexpr cigar()                          noexcept = default; //!< Defaulted.
    constexpr cigar(cigar const &)             noexcept = default; //!< Defaulted.
    constexpr cigar(cigar &&)                  noexcept = default; //!< Defaulted.
    constexpr cigar & operator=(cigar const &) noexcept = default; //!< Defaulted.
    constexpr cigar & operator=(cigar &&)      noexcept = default; //!< Defaulted.
    ~cigar()                                   noexcept = default; //!< Defaulted.

    // Inherit constructors from base
    using base_t::base_t;

    /*!\brief Construction via a value of one of the components.
     * \tparam component_type One of the component types; must be uniquely contained in the type list of the composite.
     * \param[in] alph        The value of a component that should be assigned.
     *
     * \include test/snippet/alphabet/cigar/cigar_value_construction.cpp
     */
    BIOCPP_DOXYGEN_ONLY(( constexpr cigar(component_type const alph) noexcept {} ))

    /*!\brief Assignment via a value of one of the components.
     * \tparam component_type One of the component types; must be uniquely contained in the type list of the composite.
     * \param[in] alph        The value of a component that should be assigned.
     *
     * \include test/snippet/alphabet/cigar/cigar_value_assignment.cpp
     */
    BIOCPP_DOXYGEN_ONLY(( constexpr cigar & operator=(component_type const alph) noexcept {} ))
    //!\}

    // Inherit operators from base
    using base_t::operator=;

    /*!\name Read functions
     * \{
     */
    //!\brief Return the string representation.
    small_string<11> to_string() const noexcept
    {
        small_string<11> ret{}; // maximum number of digits for uint32_t + 1 char for the cigar_op
        ret.resize(11);

        auto [ ptr, errc ] = std::to_chars(ret.data(), ret.data() + 10, get<0>(*this));

        *ptr = bio::to_char(get<1>(*this));
        (void)errc;

        ret.resize(ptr - ret.data() + 1);
        return ret;
    }
    //!\}

    /*!\name Write functions
     * \{
     */
    //!\brief Assign from the string representation.
    cigar & assign_string(small_string<11> const s) noexcept
    {
        uint32_t num{};
        auto [ ptr, errc ] = std::from_chars(s.data(), s.data() + 10, num);

        if ((errc != std::errc{}) || (!char_is_valid_for<cigar_op>(*ptr)) || (*(ptr + 1) != 0))
        {
            get<0>(*this) = 0;
            assign_char_to('P', get<1>(*this));
        }
        else
        {
            get<0>(*this) = num;
            assign_char_to(*ptr, get<1>(*this));
        }

        return *this;
    }
    //!\}

    /*!\name Get functions
     * \{
     */
    /*!\copydoc alphabet_tuple_base::get(alphabet_tuple_base & l)
     *
     * \include test/snippet/alphabet/cigar/cigar_get_index.cpp
     */
    BIOCPP_DOXYGEN_ONLY(( template <size_t index> constexpr auto get(cigar & l) noexcept {} ))

    /*!\copybrief get
     * \tparam type Return the element of specified type; only available if the type is unique in the set of components.
     * \returns A proxy to the contained element that models the same alphabet concepts and supports assignment.
     *
     * \include test/snippet/alphabet/cigar/cigar_get_type.cpp
     */
    BIOCPP_DOXYGEN_ONLY(( template <typename type> constexpr auto get(cigar & l) noexcept {} ))
    //!\}
};

} // namespace bio

#if __has_include(<fmt/format.h>)

#include <fmt/format.h>

template <>
struct fmt::formatter<bio::cigar> : fmt::formatter<std::string_view>
{
    constexpr auto format(bio::cigar const a, auto & ctx) const
    {
        auto tmp = a.to_string();
        std::string_view v{tmp.data(), tmp.size()};
        return fmt::formatter<std::string_view>::format(v, ctx);
    }
};
#endif
