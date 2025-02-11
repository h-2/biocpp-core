// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#pragma once

#include <gtest/gtest.h>

#include <iterator>
#include <ranges>

#include <bio/meta/platform.hpp>

template <typename T>
struct iterator_fixture : public ::testing::Test
{
    /* Please provide the following members:
    --------------------------------------------------------------------------------------------------------------------
    using iterator_tag = ...                                   // Defines the iterator functionality you want to test.
                                                               // One of:
                                                               // std::input_iterator_tag
                                                               // std::forward_iterator_tag
                                                               // std::bidirectional_iterator_tag
                                                               // std::random_access_iterator_tag

    static constexpr bool const_iterable = true/false;         // Also test const_iterability. (const begin/end required)

    t1 test_range;                                             // The range to test the iterators (begin/end required).
    t2 expected_range;                                         // Used to compare the iterator range with.

    --------------------------------------------------------------------------------------------------------------------
    Note: if the reference type of your iterator is not comparable via operator==() to the reference type of
          `expected_range you can additionally specify a custom expect_eq function:

    template <typename A, typename B>
    static void expect_eq(A && begin_iterator_value, B && expected_range_value)
    {
        EXPECT_EQ(begin_iterator_value, expected_range_value);
    }
    */
};

// Helper concept to check whether the test fixture has a member function expect_eq.
template <typename t>
concept has_expect_equal_member_function = requires(t & a)
{
    { t::expect_eq(*std::ranges::begin(a.test_range), *std::ranges::begin(a.expected_range)) } -> std::same_as<void>;
};

// Delegates to the test fixture member function `expect_eq` if available and falls back to EXPECT_EQ otherwise.
template <typename T, typename A, typename B>
void expext_eq(A && a, B && b)
{
    if constexpr (has_expect_equal_member_function<iterator_fixture<T>>)
        iterator_fixture<std::remove_reference_t<T>>::expect_eq(a, b);
    else
        EXPECT_EQ(a, b);
}

TYPED_TEST_SUITE_P(iterator_fixture);

TYPED_TEST_P(iterator_fixture, concept_check)
{
    using iterator_type = decltype(std::ranges::begin(this->test_range));
    // Ensure that reference types are comparable if no equal_eq function was defined.
    if constexpr (!has_expect_equal_member_function<iterator_fixture<TypeParam>>)
    {
        static_assert(std::equality_comparable_with<decltype(*std::ranges::begin(this->test_range)),
                                                    decltype(*std::ranges::begin(this->expected_range))>,
                      "The reference types of begin_iterator and expected_range must be equality comparable. "
                      "If they are not, you may specify a custom void expect_eq(i1, r2) function in the fixture.");
    }

    // input iterator must always be satisfied
    static_assert(std::input_iterator<decltype(std::ranges::begin(this->expected_range))>,
                  "expected_range must have a begin member function and "
                  "the returned iterator must model std::input_iterator.");
    EXPECT_TRUE(std::input_iterator<iterator_type>);

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::forward_iterator_tag>)
    {
        static_assert(std::forward_iterator<decltype(std::ranges::begin(this->expected_range))>,
                      "expected_range must have a begin member function and "
                      "the returned iterator must model std::forward_iterator.");
        EXPECT_TRUE(std::forward_iterator<iterator_type>);

        if constexpr (TestFixture::const_iterable)
        {
            EXPECT_TRUE(std::forward_iterator<decltype(std::ranges::cbegin(this->test_range))>);
        }
    }

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::bidirectional_iterator_tag>)
    {
        static_assert(std::bidirectional_iterator<decltype(std::ranges::begin(this->expected_range))>,
                      "expected_range must have a begin member function and "
                      "the returned iterator must model std::bidirectional_iterator.");
        EXPECT_TRUE(std::bidirectional_iterator<iterator_type>);

        if constexpr (TestFixture::const_iterable)
        {
            EXPECT_TRUE(std::bidirectional_iterator<decltype(std::ranges::cbegin(this->test_range))>);
        }
    }

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        static_assert(std::random_access_iterator<decltype(std::ranges::begin(this->expected_range))>,
                      "expected_range must have a begin member function and "
                      "the returned iterator must model std::random_access_iterator.");
        EXPECT_TRUE(std::random_access_iterator<iterator_type>);

        if constexpr (TestFixture::const_iterable)
        {
            EXPECT_TRUE(std::random_access_iterator<decltype(std::ranges::cbegin(this->test_range))>);
        }
    }

    if (!std::derived_from<typename TestFixture::iterator_tag, std::input_iterator_tag>)
    {
        FAIL() << "The iterator tag member type must be one of std::input_iterator_tag,"
               << "std::forward_iterator_tag, std::bidirectional_iterator_tag, or std::random_access_iterator_tag.";
    }
}

TYPED_TEST_P(iterator_fixture, const_non_const_compatibility)
{
    if constexpr (TestFixture::const_iterable)
    {
        using const_iterator_type = decltype(std::ranges::cbegin(this->test_range));

        const_iterator_type it{std::ranges::begin(this->test_range)};

        const_iterator_type it2{};
        it2 = std::ranges::begin(this->test_range);

        EXPECT_EQ(it, it2);
    }
}

// ---------------------------------------------------------------------------------------------------------------------
// Input & Forward Iterator
// ---------------------------------------------------------------------------------------------------------------------

TYPED_TEST_P(iterator_fixture, dereference)
{
    expext_eq<TypeParam>(*std::ranges::begin(this->test_range), *std::ranges::begin(this->expected_range));

    if constexpr (TestFixture::const_iterable)
        expext_eq<TypeParam>(*std::ranges::cbegin(this->test_range), *std::ranges::begin(this->expected_range));
}

TYPED_TEST_P(iterator_fixture, compare)
{
    EXPECT_FALSE(std::ranges::begin(this->test_range) == std::ranges::end(this->test_range));
    EXPECT_TRUE(std::ranges::begin(this->test_range)  != std::ranges::end(this->test_range));
    EXPECT_FALSE(std::ranges::end(this->test_range)   == std::ranges::begin(this->test_range));
    EXPECT_TRUE(std::ranges::end(this->test_range)    != std::ranges::begin(this->test_range));

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::forward_iterator_tag>) // iterate over it again
    {
        EXPECT_TRUE(std::ranges::begin(this->test_range)  == std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::begin(this->test_range) != std::ranges::begin(this->test_range));
    }

    if constexpr (TestFixture::const_iterable)
    {
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  == std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) != std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) == std::ranges::cend(this->test_range));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  != std::ranges::cend(this->test_range));
        EXPECT_FALSE(std::ranges::cend(this->test_range)   == std::ranges::cbegin(this->test_range));
        EXPECT_TRUE(std::ranges::cend(this->test_range)    != std::ranges::cbegin(this->test_range));

        // (non-const lhs)
        EXPECT_TRUE(std::ranges::begin(this->test_range)  == std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::begin(this->test_range) != std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::begin(this->test_range) == std::ranges::cend(this->test_range));
        EXPECT_TRUE(std::ranges::begin(this->test_range)  != std::ranges::cend(this->test_range));
        EXPECT_FALSE(std::ranges::end(this->test_range)   == std::ranges::cbegin(this->test_range));
        EXPECT_TRUE(std::ranges::end(this->test_range)    != std::ranges::cbegin(this->test_range));

        // (non-const rhs)
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  == std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) != std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::cend(this->test_range)   == std::ranges::begin(this->test_range));
        EXPECT_TRUE(std::ranges::cend(this->test_range)    != std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) == std::ranges::end(this->test_range));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  != std::ranges::end(this->test_range));
    }
}

template <typename test_type, typename it_begin_t, typename it_sentinel_t, typename rng_t>
inline void move_forward_pre_test(it_begin_t && it_begin, it_sentinel_t && it_end, rng_t && rng)
{
    // pre-increment
    auto rng_it = std::ranges::begin(rng);
    auto rng_it_end = std::ranges::end(rng);
    for (auto it = it_begin; it != it_end && rng_it != rng_it_end; ++it, ++rng_it)
        expext_eq<test_type>(*it, *rng_it);
}

template <typename test_type, typename it_begin_t, typename it_sentinel_t, typename rng_t>
inline void move_forward_post_test(it_begin_t && it_begin, it_sentinel_t && it_end, rng_t && rng)
{
    // post-increment
    auto rng_it = std::ranges::begin(rng);
    auto rng_it_end = std::ranges::end(rng);
    for (auto it = it_begin; it != it_end && rng_it != rng_it_end; it++, ++rng_it)
        expext_eq<test_type>(*it, *rng_it);
}

TYPED_TEST_P(iterator_fixture, move_forward_pre)
{
    move_forward_pre_test<TypeParam>(std::ranges::begin(this->test_range),
                                     std::ranges::end(this->test_range),
                                     this->expected_range);

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::forward_iterator_tag>) // iterate over it again
        move_forward_pre_test<TypeParam>(std::ranges::begin(this->test_range),
                                         std::ranges::end(this->test_range),
                                         this->expected_range);

    if constexpr (TestFixture::const_iterable)
        move_forward_pre_test<TypeParam>(std::ranges::cbegin(this->test_range),
                                         std::ranges::cend(this->test_range),
                                         this->expected_range);
}

TYPED_TEST_P(iterator_fixture, move_forward_post)
{
    move_forward_post_test<TypeParam>(std::ranges::begin(this->test_range),
                                      std::ranges::end(this->test_range),
                                      this->expected_range);

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::forward_iterator_tag>) // iterate over it again
        move_forward_post_test<TypeParam>(std::ranges::begin(this->test_range),
                                          std::ranges::end(this->test_range),
                                          this->expected_range);

    if constexpr (TestFixture::const_iterable)
        move_forward_post_test<TypeParam>(std::ranges::cbegin(this->test_range),
                                          std::ranges::cend(this->test_range),
                                          this->expected_range);
}

// ---------------------------------------------------------------------------------------------------------------------
// Bidirectional Iterator
// ---------------------------------------------------------------------------------------------------------------------

template <typename it_begin_t, typename it_sentinel_t, typename rng_t>
inline auto last_iterators(it_begin_t const & it_begin, it_sentinel_t const & it_end, rng_t && rng)
{
    it_begin_t it = it_begin;
    auto rng_it = std::ranges::begin(rng);

    for (auto const rng_it_end = std::ranges::end(rng);
         std::ranges::next(it) != it_end && std::ranges::next(rng_it) != rng_it_end;
         ++it, ++rng_it);

    return std::pair{it, rng_it};
}

template <typename test_type, typename it_begin_t, typename it_sentinel_t, typename rng_t>
inline void move_backward_pre_test(it_begin_t && it_begin, it_sentinel_t && it_end, rng_t && rng)
{
    // move to last position
    auto && [last_it, rng_last_it] = last_iterators(it_begin, it_end, rng);
    auto const rng_it_begin = std::ranges::begin(rng);

    // pre-decrement
    for (auto it = last_it, rng_it = rng_last_it;
         it != it_begin && rng_it != rng_it_begin;
         --rng_it)
    {
       expext_eq<test_type>(*it, *rng_it);
       --it;
    }

    expext_eq<test_type>(*it_begin, *rng_it_begin);
}

template <typename test_type, typename it_begin_t, typename it_sentinel_t, typename rng_t>
inline void move_backward_post_test(it_begin_t && it_begin, it_sentinel_t && it_end, rng_t && rng)
{
    // move to last position
    auto && [last_it, rng_last_it] = last_iterators(it_begin, it_end, rng);
    auto const rng_it_begin = std::ranges::begin(rng);

    // post-decrement
    for (auto it = last_it, rng_it = rng_last_it;
         it != it_begin && rng_it != rng_it_begin;
         --rng_it)
    {
       expext_eq<test_type>(*(it--), *rng_it);
    }

    expext_eq<test_type>(*it_begin, *rng_it_begin);
}

TYPED_TEST_P(iterator_fixture, move_backward_pre)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::bidirectional_iterator_tag>)
    {
        move_backward_pre_test<TypeParam>(std::ranges::begin(this->test_range),
                                          std::ranges::end(this->test_range),
                                          this->expected_range);

        if constexpr (TestFixture::const_iterable)
            move_backward_pre_test<TypeParam>(std::ranges::cbegin(this->test_range),
                                              std::ranges::cend(this->test_range),
                                              this->expected_range);
    }
}

TYPED_TEST_P(iterator_fixture, move_backward_post)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::bidirectional_iterator_tag>)
    {
        move_backward_post_test<TypeParam>(std::ranges::begin(this->test_range),
                                           std::ranges::end(this->test_range),
                                           this->expected_range);

        if constexpr (TestFixture::const_iterable)
            move_backward_post_test<TypeParam>(std::ranges::cbegin(this->test_range),
                                               std::ranges::cend(this->test_range),
                                               this->expected_range);
    }
}

// ---------------------------------------------------------------------------------------------------------------------
// Random Access Iterator
// ---------------------------------------------------------------------------------------------------------------------

template <typename test_type, typename it_begin_t, typename rng_t>
inline void jump_forward_test(it_begin_t && it_begin, rng_t && rng)
{
    size_t sz = std::ranges::distance(rng);

    // Forward
    for (size_t n = 0; n < sz; ++n)
    {
        auto it = it_begin;
        expext_eq<test_type>(rng[n], *(it += n));
        expext_eq<test_type>(rng[n], *(it));
    }

    // Forward copy
    for (size_t n = 0; n < sz; ++n)
    {
        expext_eq<test_type>(rng[n], *(it_begin + n));
        expext_eq<test_type>(rng[0], *it_begin);
    }

    // Forward copy friend
    for (size_t n = 0; n < sz; ++n)
    {
        expext_eq<test_type>(rng[n], *(n + it_begin));
        expext_eq<test_type>(rng[0], *it_begin);
    }
}

TYPED_TEST_P(iterator_fixture, jump_forward)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        jump_forward_test<TypeParam>(std::ranges::begin(this->test_range), this->expected_range);

        if constexpr (TestFixture::const_iterable)
            jump_forward_test<TypeParam>(std::ranges::cbegin(this->test_range), this->expected_range);
    }
}

template <typename test_type, typename it_begin_t, typename rng_t>
inline void jump_backward_test(it_begin_t && it_begin, rng_t && rng)
{
    size_t sz = std::ranges::distance(rng);

    auto pre_end_it = it_begin + (sz - 1);

    // Backward
    for (size_t n = 0; n < sz; ++n)
    {
        auto it = pre_end_it;
        expext_eq<test_type>(rng[sz - 1 - n], *(it -= n));
        expext_eq<test_type>(rng[sz - 1 - n], *it);
    }

    // Backward copy
    for (size_t n = 0; n < sz; ++n)
    {
        expext_eq<test_type>(rng[sz - n - 1], *(pre_end_it - n));
        expext_eq<test_type>(rng[sz - 1], *pre_end_it);
    }

    // Backward copy friend through (-n) + it
    for (size_t n = 0; n < sz; ++n)
    {
        expext_eq<test_type>(rng[sz - n - 1], *((-1 * n) + pre_end_it));
        expext_eq<test_type>(rng[sz - 1], *pre_end_it);
    }
}

TYPED_TEST_P(iterator_fixture, jump_backward)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        jump_backward_test<TypeParam>(std::ranges::begin(this->test_range), this->expected_range);

        if constexpr (TestFixture::const_iterable)
            jump_backward_test<TypeParam>(std::ranges::cbegin(this->test_range), this->expected_range);
    }
}

template <typename test_type, typename it_begin_t, typename rng_t>
inline void jump_random_test(it_begin_t && it_begin, rng_t && rng)
{
    size_t sz = std::ranges::distance(rng);

    for (size_t n = 0; n < sz; ++n)
        expext_eq<test_type>(rng[n], it_begin[n]);
}

TYPED_TEST_P(iterator_fixture, jump_random)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        jump_random_test<TypeParam>(std::ranges::begin(this->test_range), this->expected_range);

        if constexpr (TestFixture::const_iterable)
            jump_random_test<TypeParam>(std::ranges::cbegin(this->test_range), this->expected_range);
    }
}

template <typename it_begin_t, typename rng_t>
inline void difference_test(it_begin_t && it_begin, rng_t && rng)
{
    size_t sz = std::ranges::distance(rng);
    using difference_t = std::iter_difference_t<it_begin_t>;

    for (size_t n = 0; n < sz; ++n)
        EXPECT_EQ(static_cast<difference_t>(n), ((it_begin + n) - it_begin));
}

TYPED_TEST_P(iterator_fixture, difference_common)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        difference_test(std::ranges::begin(this->test_range), this->expected_range);

        if constexpr (TestFixture::const_iterable)
            difference_test(std::ranges::cbegin(this->test_range), this->expected_range);
    }
}

TYPED_TEST_P(iterator_fixture, difference_sentinel)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        using difference_t = std::ranges::range_difference_t<decltype(this->test_range)>;

        auto && begin = std::ranges::begin(this->test_range);
        auto && end = std::ranges::end(this->test_range);
        difference_t size = std::ranges::distance(this->expected_range);

        EXPECT_EQ(size, end - begin);
        EXPECT_EQ(-size, begin - end);

        if constexpr (TestFixture::const_iterable)
        {
            auto && cbegin = std::ranges::cbegin(this->test_range);
            auto && cend = std::ranges::cend(this->test_range);

            EXPECT_EQ(size, cend - cbegin);
            EXPECT_EQ(-size, cbegin - cend);

            EXPECT_EQ(size, end - cbegin);
            EXPECT_EQ(-size, cbegin - end);

            EXPECT_EQ(size, cend - begin);
            EXPECT_EQ(-size, begin - cend);
        }
    }
}

TYPED_TEST_P(iterator_fixture, compare_less)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        EXPECT_FALSE(std::ranges::begin(this->test_range) < std::ranges::begin(this->test_range));
        EXPECT_TRUE(std::ranges::begin(this->test_range)  < std::ranges::next(std::ranges::begin(this->test_range)));
    }

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag> &&
                  TestFixture::const_iterable)
    {
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) < std::ranges::cbegin(this->test_range));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  < std::ranges::next(std::ranges::cbegin(this->test_range)));

        // mix
        EXPECT_FALSE(std::ranges::begin(this->test_range)  < std::ranges::cbegin(this->test_range));
        EXPECT_TRUE(std::ranges::begin(this->test_range)   < std::ranges::next(std::ranges::cbegin(this->test_range)));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) < std::ranges::begin(this->test_range));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  < std::ranges::next(std::ranges::begin(this->test_range)));
    }
}

TYPED_TEST_P(iterator_fixture, compare_greater)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        EXPECT_FALSE(std::ranges::begin(this->test_range) > std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::begin(this->test_range) > std::ranges::next(std::ranges::begin(this->test_range)));
    }

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag> &&
                  TestFixture::const_iterable)
    {
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) > std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) > std::ranges::next(std::ranges::cbegin(this->test_range)));

        // mix
        EXPECT_FALSE(std::ranges::begin(this->test_range)  > std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::begin(this->test_range)  > std::ranges::next(std::ranges::cbegin(this->test_range)));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) > std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) > std::ranges::next(std::ranges::begin(this->test_range)));
    }
}

TYPED_TEST_P(iterator_fixture, compare_leq)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        EXPECT_TRUE(std::ranges::begin(this->test_range) <= std::ranges::begin(this->test_range));
        EXPECT_TRUE(std::ranges::begin(this->test_range) <= std::ranges::next(std::ranges::begin(this->test_range)));
    }

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag> &&
                  TestFixture::const_iterable)
    {
        EXPECT_TRUE(std::ranges::cbegin(this->test_range) <= std::ranges::cbegin(this->test_range));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range) <= std::ranges::next(std::ranges::cbegin(this->test_range)));

        // mix
        EXPECT_TRUE(std::ranges::begin(this->test_range)  <= std::ranges::cbegin(this->test_range));
        EXPECT_TRUE(std::ranges::begin(this->test_range)  <= std::ranges::next(std::ranges::cbegin(this->test_range)));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range) <= std::ranges::begin(this->test_range));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range) <= std::ranges::next(std::ranges::begin(this->test_range)));
    }
}

TYPED_TEST_P(iterator_fixture, compare_geq)
{
    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag>)
    {
        EXPECT_TRUE(std::ranges::begin(this->test_range)  >= std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::begin(this->test_range) >= std::ranges::next(std::ranges::begin(this->test_range)));
    }

    if constexpr (std::derived_from<typename TestFixture::iterator_tag, std::random_access_iterator_tag> &&
                  TestFixture::const_iterable)
    {
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  >= std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) >= std::ranges::next(std::ranges::cbegin(this->test_range)));

        // mix
        EXPECT_TRUE(std::ranges::begin(this->test_range)   >= std::ranges::cbegin(this->test_range));
        EXPECT_FALSE(std::ranges::begin(this->test_range)  >= std::ranges::next(std::ranges::cbegin(this->test_range)));
        EXPECT_TRUE(std::ranges::cbegin(this->test_range)  >= std::ranges::begin(this->test_range));
        EXPECT_FALSE(std::ranges::cbegin(this->test_range) >= std::ranges::next(std::ranges::begin(this->test_range)));
    }
}

REGISTER_TYPED_TEST_SUITE_P(iterator_fixture,
                            concept_check,
                            const_non_const_compatibility,
                            dereference,
                            compare,
                            move_forward_pre,
                            move_forward_post,
                            move_backward_pre,
                            move_backward_post,
                            jump_forward,
                            jump_backward,
                            jump_random,
                            difference_common,
                            difference_sentinel,
                            compare_less,
                            compare_greater,
                            compare_leq,
                            compare_geq);
