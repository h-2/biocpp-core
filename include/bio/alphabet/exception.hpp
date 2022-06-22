// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \author Hannes Hauswedell <hannes.hauswedell AT decode.is>
 * \brief Exceptions thrown by entities in the alphabet module.
 */

#pragma once

#include <concepts>
#include <stdexcept>
#include <string>

#include <bio/meta/platform.hpp>

namespace bio
{

//!\brief An exception typically thrown by bio::alphabet::assign_char_strict.
struct invalid_char_assignment : std::runtime_error
{
    //!\brief Constructor that takes the type name and the failed character as arguments.
    invalid_char_assignment(std::string const & type_name, char const wrong_char) :
      std::runtime_error{std::string{"Assigning "} +
                         (std::isprint(wrong_char) ? std::string("\"") + std::string(1, wrong_char) + "\""
                                                   : std::to_string((int)wrong_char)) +
                         " to an alphabet of type " + type_name +
                         " would incur information loss. If you want implicit conversion, use "
                         "bio::assign_char instead of bio::assign_char_strict."}
    {}

    //!\overload
    invalid_char_assignment(std::string const & type_name, std::convertible_to<char> auto const wrong_char) :
      invalid_char_assignment(type_name, static_cast<char>(wrong_char))
    {}
};

} // namespace bio
