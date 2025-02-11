// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \author Marie Hoffmann <marie.hoffmann AT fu-berlin.de>
 * \brief Provides bio::phred42 quality scores.
 */

#pragma once

#include <bio/alphabet/quality/quality_base.hpp>

// ------------------------------------------------------------------
// phred42
// ------------------------------------------------------------------

namespace bio
{

/*!\brief Quality type for traditional Sanger and modern Illumina Phred scores (typical range).
 * \implements bio::writable_quality_alphabet
 * \if DEV \implements bio::detail::writable_constexpr_alphabet \endif
 * \implements bio::trivially_copyable
 * \implements bio::standard_layout
 * \implements std::regular
 *
 * \ingroup quality
 *
 * \details
 *
 * The phred42 quality alphabet represents the zero-based phred score range
 * [0..41] mapped to the consecutive ASCII range ['!' .. 'J']. It therefore can
 * represent the Illumina 1.8+ standard and the original Sanger score. If you
 * intend to use phred scores exceeding 41, use the larger score type, namely
 * bio::phred63, otherwise on construction exceeding scores are mapped to 41.
 *
 * \include test/snippet/alphabet/quality/phred42.cpp
 */
class phred42 : public quality_base<phred42, 42>
{
private:
    //!\brief The base class.
    using base_t = quality_base<phred42, 42>;

    //!\brief Befriend bio::quality_base.
    friend base_t;
    //!\cond \brief Befriend bio::alphabet_base.
    friend base_t::base_t;
    //!\endcond

public:
    /*!\name Constructors, destructor and assignment
     * \{
     */
    constexpr phred42()                             noexcept = default; //!< Defaulted.
    constexpr phred42(phred42 const &)              noexcept = default; //!< Defaulted.
    constexpr phred42(phred42 &&)                   noexcept = default; //!< Defaulted.
    constexpr phred42 & operator=(phred42 const &)  noexcept = default; //!< Defaulted.
    constexpr phred42 & operator=(phred42 &&)       noexcept = default; //!< Defaulted.
    ~phred42()                                      noexcept = default; //!< Defaulted.

    //!\brief Construct from phred value.
    constexpr phred42(phred_type const p) : base_t{p} {}

    // Inherit converting constructor
    using base_t::base_t;
    //!\}

    /*!\name Member variables.
     * \{
     */
    //!\brief The projection offset between phred and rank score representation.
    static constexpr phred_type offset_phred{0};

    //!\brief The projection offset between char and rank score representation.
    static constexpr char_type offset_char{'!'};
    //!\}
};

/*!\name Literals
 * \{
 */
/*!\brief The bio::phred42 char literal.
 * \relates bio::phred42
 * \returns bio::phred42
 */
constexpr phred42 operator""_phred42(char const c) noexcept
{
    return phred42{}.assign_char(c);
}

/*!\brief The bio::phred42 string literal.
 * \param[in] s A pointer to the character sequence to assign from.
 * \param[in] n The length of the character sequence to assign from.
 * \relates bio::phred42
 * \returns bio::std::vector<bio::phred42>
 *
 * You can use this string literal to easily assign to std::vector<bio::phred42>:
 *
 * \include test/snippet/alphabet/quality/phred42_literal.cpp
 */
inline std::vector<phred42> operator""_phred42(char const * s, std::size_t n)
{
    std::vector<phred42> r;
    r.resize(n);

    for (size_t i = 0; i < n; ++i)
        r[i].assign_char(s[i]);

    return r;
}
//!\}

} // namespace bio
