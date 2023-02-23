// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \author Marie Hoffmann <marie.hoffmann AT fu-berlin.de>
 * \brief Provides bio::alphabet::phred42 quality scores.
 */

#pragma once

#include <bio/alphabet/base.hpp>
#include <bio/alphabet/detail/convert.hpp>
#include <bio/alphabet/quality/concept.hpp>

namespace bio::alphabet
{

/*!\brief A CRTP-base that refines bio::alphabet::base and is used by the quality alphabets.
 * \ingroup quality
 * \tparam derived_type The CRTP parameter type.
 * \tparam size         The size of the alphabet.
 */
template <typename derived_type, auto size>
class quality_base : public base<derived_type, size, char>
{
public:
    /*!\name Member types
     * \{
     */
    //!\brief The integer representation of a quality score assignable with =operator.
    using phred_type = int8_t;
    //!\}

private:
    //!\brief The base type.
    using base_t = base<derived_type, size, char>;

    /*!\name Constructors, destructor and assignment
     * \{
     */
    constexpr quality_base() noexcept                                 = default; //!< Defaulted.
    constexpr quality_base(quality_base const &) noexcept             = default; //!< Defaulted.
    constexpr quality_base(quality_base &&) noexcept                  = default; //!< Defaulted.
    constexpr quality_base & operator=(quality_base const &) noexcept = default; //!< Defaulted.
    constexpr quality_base & operator=(quality_base &&) noexcept      = default; //!< Defaulted.
    ~quality_base() noexcept                                          = default; //!< Defaulted.

    //!\brief Allow construction from the phred value.
    constexpr quality_base(phred_type const p) noexcept { static_cast<derived_type *>(this)->assign_phred(p); }
    //!\}

    //!\brief Befriend the derived_type so it can instantiate.
    friend derived_type;

public:
    // Import from base type:
    using base_t::alphabet_size;
    using base_t::assign_rank;
    using base_t::to_rank;
    using typename base_t::char_type;
    using typename base_t::rank_type;

    /*!\name Constructors, destructor and assignment
     * \{
     */
    // This constructor needs to be public, because constructor templates are not inherited otherwise
    //!\brief Allow explicit construction from any other quality type by means of the phred representation.
    template <meta::different_from<derived_type> other_qual_type>
    //!\cond
        requires quality<other_qual_type>
    //!\endcond
    explicit constexpr quality_base(other_qual_type const & other) noexcept
    {
        assign_phred_to(bio::alphabet::to_phred(other), static_cast<derived_type &>(*this));
    }
    //!\}

    /*!\name Read functions
     * \{
     */
    //!\brief Return the alphabet's value in phred representation.
    constexpr phred_type to_phred() const noexcept { return rank_to_phred[to_rank()]; }
    //!\}

    /*!\name Write functions
     * \{
     */

    /*!\brief Assign from the numeric phred value.
     *
     * \details
     *
     * Satisfies the bio::alphabet::writable_quality::assign_phred() requirement via the bio::alphabet::assign_rank() wrapper.
     *
     * ### Complexity
     *
     * Constant.
     */
    constexpr derived_type & assign_phred(phred_type const p) noexcept
    {
        return assign_rank(phred_to_rank[static_cast<rank_type>(p)]);
    }
    //!\}

protected:
    //!\brief Phred to rank conversion table.
    static constexpr std::array<rank_type, 256> phred_to_rank = []() constexpr
    {
        std::array<rank_type, 256> ret{};

        for (int64_t i = std::numeric_limits<phred_type>::lowest(); i <= std::numeric_limits<phred_type>::max(); ++i)
        {
            if (i < derived_type::offset_phred) // map too-small to smallest possible
                ret[static_cast<rank_type>(i)] = 0;
            else if (i >= derived_type::offset_phred +
                            static_cast<int64_t>(alphabet_size)) // map too-large to highest possible
                ret[static_cast<rank_type>(i)] = alphabet_size - 1;
            else                                                 // map valid range to identity
                ret[static_cast<rank_type>(i)] = i - derived_type::offset_phred;
        }
        return ret;
    }();

    //!\brief Char to rank conversion table.
    static constexpr std::array<rank_type, 256> char_to_rank = []() constexpr
    {
        std::array<rank_type, 256> ret{};

        for (int64_t i = std::numeric_limits<char_type>::lowest(); i <= std::numeric_limits<char_type>::max(); ++i)
        {
            if (i < derived_type::offset_char) // map too-small to smallest possible
                ret[static_cast<rank_type>(i)] = 0;
            else if (i >= derived_type::offset_char +
                            static_cast<int64_t>(alphabet_size)) // map too-large to highest possible
                ret[static_cast<rank_type>(i)] = alphabet_size - 1;
            else                                                 // map valid range to identity
                ret[static_cast<rank_type>(i)] = i - derived_type::offset_char;
        }

        return ret;
    }();

    //!\brief Rank to phred conversion table.
    static constexpr std::array<phred_type, alphabet_size> rank_to_phred = []() constexpr
    {
        std::array<phred_type, alphabet_size> ret{};

        for (size_t i = 0; i < alphabet_size; ++i)
            ret[i] = i + derived_type::offset_phred;

        return ret;
    }();

    //!\brief Rank to char conversion table.
    static constexpr std::array<char_type, alphabet_size> rank_to_char = []() constexpr
    {
        std::array<char_type, alphabet_size> ret{};

        for (size_t i = 0; i < alphabet_size; ++i)
            ret[i] = i + derived_type::offset_char;

        return ret;
    }();

    //!\brief tag_invoke() wrapper around member.
    friend constexpr phred_type tag_invoke(custom::to_phred, derived_type const alph) noexcept
    {
        return alph.to_phred();
    }

    //!\brief tag_invoke() wrapper around member.
    friend constexpr derived_type & tag_invoke(custom::assign_phred_to,
                                               phred_type const p,
                                               derived_type &   alph) noexcept
    {
        return alph.assign_phred(p);
    }
};

} // namespace bio::alphabet
