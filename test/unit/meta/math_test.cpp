// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <gtest/gtest.h>

#include <bio/meta/math.hpp>

static constexpr size_t max_iterations = 1;//1 << 15;

using unsigned_types = ::testing::Types<uint8_t, uint16_t, uint32_t, uint64_t>;

template <typename type>
class unsigned_operations : public ::testing::Test
{};

TYPED_TEST_SUITE(unsigned_operations, unsigned_types, );

TYPED_TEST(unsigned_operations, floor_log2)
{
    using unsigned_t = TypeParam;
    constexpr size_t zero = bio::detail::floor_log2<unsigned_t>(0b0001);
    constexpr size_t one1 = bio::detail::floor_log2<unsigned_t>(0b0010);
    constexpr size_t one2 = bio::detail::floor_log2<unsigned_t>(0b0011);
    constexpr size_t two1 = bio::detail::floor_log2<unsigned_t>(0b0101);
    constexpr size_t two2 = bio::detail::floor_log2<unsigned_t>(0b0111);
    constexpr size_t seven = bio::detail::floor_log2<unsigned_t>(0b10010010);
    EXPECT_EQ(zero, 0u);
    EXPECT_EQ(one1, 1u);
    EXPECT_EQ(one2, 1u);
    EXPECT_EQ(two1, 2u);
    EXPECT_EQ(two2, 2u);
    EXPECT_EQ(seven, 7u);

    for (uint8_t log2_value = 0; log2_value < bio::detail::sizeof_bits<unsigned_t>; ++log2_value)
    {
        unsigned_t start = unsigned_t{1u} << log2_value;
        unsigned_t end = start << 1u;
        for (unsigned_t n = start, k = 0u; n < end && k < max_iterations; ++n, ++k)
        {
            EXPECT_EQ(bio::detail::floor_log2(n), log2_value);
            EXPECT_EQ(std::floor(std::log2(n)), log2_value) << "If this fails this might be a floating point rounding "
                                                            << "error on your machine";
        }
    }
}

TYPED_TEST(unsigned_operations, ceil_log2)
{
    using unsigned_t = TypeParam;
    constexpr size_t zero = bio::detail::ceil_log2<unsigned_t>(0b0001);
    constexpr size_t one = bio::detail::ceil_log2<unsigned_t>(0b0010);
    constexpr size_t two = bio::detail::ceil_log2<unsigned_t>(0b0011);
    constexpr size_t three1 = bio::detail::ceil_log2<unsigned_t>(0b0101);
    constexpr size_t three2 = bio::detail::ceil_log2<unsigned_t>(0b0111);
    constexpr size_t eight = bio::detail::ceil_log2<unsigned_t>(0b10010010);
    EXPECT_EQ(zero, 0u);
    EXPECT_EQ(one, 1u);
    EXPECT_EQ(two, 2u);
    EXPECT_EQ(three1, 3u);
    EXPECT_EQ(three2, 3u);
    EXPECT_EQ(eight, 8u);

    for (uint8_t log2_value = 0; log2_value < bio::detail::sizeof_bits<unsigned_t>; ++log2_value)
    {
        unsigned_t start = unsigned_t{1u} << log2_value;
        unsigned_t end = start << 1u;
        EXPECT_EQ(bio::detail::ceil_log2(start), log2_value);
        EXPECT_EQ(std::ceil(std::log2(start)), log2_value) << "ceil_log2 of " << start << " should be " << log2_value
                                                           << "; If this fails this might be a floating point rounding "
                                                           << "error on your machine.";

        for (unsigned_t n = start + 1u, k = 0u; n < end && k < max_iterations; ++n, ++k)
        {
            EXPECT_EQ(bio::detail::ceil_log2(n), log2_value + 1u);

            if constexpr (bio::detail::sizeof_bits<unsigned_t> <= 32u) // known to fail for 64bit unsigned integers
            {
                EXPECT_EQ(std::ceil(std::log2(n)), log2_value + 1u) << "ceil_log2 of " << start << " should be "
                                                                    << log2_value
                                                                    << "; If this fails this might be a floating point"
                                                                    << "rounding error on your machine.";
            }
        }
    }
}

TEST(pow, unsigned_base)
{
    EXPECT_EQ(0u, bio::pow(0u, 2u));
    EXPECT_EQ(1u, bio::pow(2u, 0u));
    EXPECT_EQ(8u, bio::pow(2u, 3u));
    EXPECT_EQ(std::numeric_limits<uint64_t>::max(), bio::pow(std::numeric_limits<uint64_t>::max(), 1u));
}

TEST(pow, signed_base)
{
    EXPECT_EQ(0, bio::pow(0, 2u));
    EXPECT_EQ(1, bio::pow(2, 0u));
    EXPECT_EQ(8, bio::pow(2, 3u));
    EXPECT_EQ(-8, bio::pow(-2, 3u));
    EXPECT_EQ(std::numeric_limits<int64_t>::max(), bio::pow(std::numeric_limits<int64_t>::max(), 1u));
    EXPECT_EQ(std::numeric_limits<int64_t>::min(), bio::pow(std::numeric_limits<int64_t>::min(), 1u));
}

TEST(pow, std)
{
    EXPECT_EQ(0.0, bio::pow(0u, 2));
    EXPECT_EQ(1.0, bio::pow(2, 0));
    EXPECT_EQ(27.0, bio::pow(3.0, 3u));
    EXPECT_EQ(-8.0, bio::pow(-2.0, 3));
}

#ifndef NDEBUG
TEST(pow, overflow)
{
    EXPECT_THROW(bio::pow(2u, 64u), std::overflow_error);
    EXPECT_THROW(bio::pow(2, 63u), std::overflow_error);
}

TEST(pow, underflow)
{
    EXPECT_THROW(bio::pow(-3, 50u), std::underflow_error);
}
#endif
