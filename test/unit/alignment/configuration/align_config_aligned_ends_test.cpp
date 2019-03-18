// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2006-2019, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2019, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/seqan/seqan3/blob/master/LICENSE
// -----------------------------------------------------------------------------------------------------

#include <gtest/gtest.h>

#include <functional>
#include <random>
#include <type_traits>

#include <seqan3/core/metafunction/function.hpp>
#include <seqan3/alignment/configuration/align_config_aligned_ends.hpp>
#include <seqan3/core/algorithm/configuration.hpp>

using namespace seqan3;
using namespace seqan3::align_cfg;

template <typename value_t>
struct dummy_gap : sequence_end_gap_specifier_base<value_t>
{};

template <typename type>
class static_end_gap_test : public ::testing::Test
{
public:

    static constexpr bool expected_value = static_end_gap_test::determine_value(type{});

private:

    template <template <typename> typename gap_element,
              typename value_t>
    static constexpr bool determine_value(gap_element<value_t>)
    {
        return value_t::value;
    }
};

using static_end_gap_types = ::testing::Types<front_end_first<std::true_type>,
                                              front_end_first<std::false_type>,
                                              back_end_first<std::true_type>,
                                              back_end_first<std::false_type>,
                                              second_seq_leading<std::true_type>,
                                              second_seq_leading<std::false_type>,
                                              second_seq_trailing<std::true_type>,
                                              second_seq_trailing<std::false_type>>;

TYPED_TEST_CASE(static_end_gap_test, static_end_gap_types);

template <typename type>
class dynamic_end_gap_test : public ::testing::Test
{};

using dynamic_end_gap_types = ::testing::Types<front_end_first<bool>,
                                               back_end_first<bool>,
                                               second_seq_leading<bool>,
                                               second_seq_trailing<bool>>;

TYPED_TEST_CASE(dynamic_end_gap_test, dynamic_end_gap_types);

TEST(sequence_end_gap_specifier_base, aggregate)
{
    EXPECT_TRUE((std::is_aggregate_v<dummy_gap<std::true_type>>));
    EXPECT_TRUE((std::is_aggregate_v<dummy_gap<bool>>));
}

TEST(front_end_first, deduction)
{
    { // static
        front_end_first tmp{std::true_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp), front_end_first<std::true_type>>), true);
        front_end_first tmp2{std::false_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), front_end_first<std::false_type>>), true);
    }

    { // dynamic
        front_end_first tmp{true};
        EXPECT_EQ((std::is_same_v<decltype(tmp), front_end_first<bool>>), true);
        front_end_first tmp2{false};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), front_end_first<bool>>), true);
    }
}

TEST(back_end_first, deduction)
{
    { // static
        back_end_first tmp{std::true_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp), back_end_first<std::true_type>>), true);
        back_end_first tmp2{std::false_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), back_end_first<std::false_type>>), true);
    }

    { // dynamic
        back_end_first tmp{true};
        EXPECT_EQ((std::is_same_v<decltype(tmp), back_end_first<bool>>), true);
        back_end_first tmp2{false};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), back_end_first<bool>>), true);
    }
}

TEST(second_seq_leading, deduction)
{
    { // static
        second_seq_leading tmp{std::true_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp), second_seq_leading<std::true_type>>), true);
        second_seq_leading tmp2{std::false_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), second_seq_leading<std::false_type>>), true);
    }

    { // dynamic
        second_seq_leading tmp{true};
        EXPECT_EQ((std::is_same_v<decltype(tmp), second_seq_leading<bool>>), true);
        second_seq_leading tmp2{false};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), second_seq_leading<bool>>), true);
    }
}

TEST(second_seq_trailing, deduction)
{
    { // static
        second_seq_trailing tmp{std::true_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp), second_seq_trailing<std::true_type>>), true);
        second_seq_trailing tmp2{std::false_type{}};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), second_seq_trailing<std::false_type>>), true);
    }

    { // dynamic
        second_seq_trailing tmp{true};
        EXPECT_EQ((std::is_same_v<decltype(tmp), second_seq_trailing<bool>>), true);
        second_seq_trailing tmp2{false};
        EXPECT_EQ((std::is_same_v<decltype(tmp2), second_seq_trailing<bool>>), true);
    }
}

TYPED_TEST(static_end_gap_test, aggreagte)
{
    EXPECT_TRUE((std::is_aggregate_v<TypeParam>));
}

TYPED_TEST(static_end_gap_test, get_value)
{
    TypeParam instance{};
    EXPECT_EQ(instance(), TestFixture::expected_value);
}

TYPED_TEST(dynamic_end_gap_test, construction)
{
    EXPECT_TRUE((std::is_aggregate_v<TypeParam>));
}

TYPED_TEST(dynamic_end_gap_test, get_value)
{
    { // true
        TypeParam instance{true};
        EXPECT_EQ(instance(), true);
    }
    { // false
        TypeParam instance{false};
        EXPECT_EQ(instance(), false);
    }
}

TEST(end_gaps, construction)
{
    { // empty
        EXPECT_TRUE((std::is_nothrow_default_constructible_v<end_gaps<>>));
        EXPECT_TRUE((std::is_nothrow_copy_constructible_v<end_gaps<>>));
        EXPECT_TRUE((std::is_nothrow_move_constructible_v<end_gaps<>>));
        EXPECT_TRUE((std::is_nothrow_copy_assignable_v<end_gaps<>>));
        EXPECT_TRUE((std::is_nothrow_move_assignable_v<end_gaps<>>));
    }

    { // one element
        EXPECT_TRUE((std::is_nothrow_default_constructible_v<end_gaps<front_end_first<std::true_type>>>));
        EXPECT_TRUE((std::is_nothrow_copy_constructible_v<end_gaps<front_end_first<std::true_type>>>));
        EXPECT_TRUE((std::is_nothrow_move_constructible_v<end_gaps<front_end_first<std::true_type>>>));
        EXPECT_TRUE((std::is_nothrow_copy_assignable_v<end_gaps<front_end_first<std::true_type>>>));
        EXPECT_TRUE((std::is_nothrow_move_assignable_v<end_gaps<front_end_first<std::true_type>>>));

        EXPECT_TRUE((std::is_nothrow_constructible_v<end_gaps<front_end_first<std::true_type>>>));
    }

    { // four elements
        EXPECT_TRUE((std::is_nothrow_default_constructible_v<end_gaps<front_end_first<std::true_type>,
                                                                      second_seq_leading<bool>,
                                                                      back_end_first<std::false_type>,
                                                                      second_seq_trailing<bool>>>));
        EXPECT_TRUE((std::is_nothrow_copy_constructible_v<end_gaps<front_end_first<std::true_type>,
                                                                   second_seq_leading<bool>,
                                                                   back_end_first<std::false_type>,
                                                                   second_seq_trailing<bool>>>));
        EXPECT_TRUE((std::is_nothrow_move_constructible_v<end_gaps<front_end_first<std::true_type>,
                                                                   second_seq_leading<bool>,
                                                                   back_end_first<std::false_type>,
                                                                   second_seq_trailing<bool>>>));
        EXPECT_TRUE((std::is_nothrow_copy_assignable_v<end_gaps<front_end_first<std::true_type>,
                                                                second_seq_leading<bool>,
                                                                back_end_first<std::false_type>,
                                                                second_seq_trailing<bool>>>));
        EXPECT_TRUE((std::is_nothrow_move_assignable_v<end_gaps<front_end_first<std::true_type>,
                                                                second_seq_leading<bool>,
                                                                back_end_first<std::false_type>,
                                                                second_seq_trailing<bool>>>));
        EXPECT_TRUE((std::is_nothrow_constructible_v<end_gaps<front_end_first<std::true_type>,
                                                              second_seq_leading<bool>,
                                                              back_end_first<std::false_type>,
                                                              second_seq_trailing<bool>>>));
    }

    { // from lvalue
        front_end_first fsl{std::true_type{}};
        EXPECT_TRUE(end_gaps{fsl}[0]);
    }
}

TEST(end_gaps, deduction)
{
    { // default
        end_gaps eg{};
        EXPECT_EQ((std::is_same_v<decltype(eg), end_gaps<>>), true);
    }

    { // one element
        end_gaps eg{second_seq_trailing{true}};
        using foo = end_gaps<second_seq_trailing<bool>>;
        EXPECT_EQ((std::is_same_v<decltype(eg), foo>), true);
    }

    { // multiple elements
        end_gaps eg{second_seq_trailing{true}, front_end_first{std::true_type{}}, second_seq_leading{std::false_type{}}};
        using foo = end_gaps<second_seq_trailing<bool>, front_end_first<std::true_type>, second_seq_leading<std::false_type>>;
        EXPECT_EQ((std::is_same_v<decltype(eg), foo>), true);
    }
}

TEST(end_gaps, access)
{
    { // default
        end_gaps eg{};

        EXPECT_EQ(eg[0], false);
        EXPECT_EQ(eg[1], false);
        EXPECT_EQ(eg[2], false);
        EXPECT_EQ(eg[3], false);
    }

    { // custom
        end_gaps eg{second_seq_trailing{true}, front_end_first{std::true_type{}}, second_seq_leading{std::false_type{}}};

        EXPECT_EQ(eg[0], true);
        EXPECT_EQ(eg[1], false);
        EXPECT_EQ(eg[2], false);
        EXPECT_EQ(eg[3], true);
    }
}

TEST(end_gaps, static_query)
{
    { // default
        end_gaps eg{};

        constexpr bool seq1_l = decltype(eg)::is_static<0>();
        constexpr bool seq1_t = decltype(eg)::is_static<1>();
        constexpr bool seq2_l = decltype(eg)::is_static<2>();
        constexpr bool seq2_t = decltype(eg)::is_static<3>();
        EXPECT_EQ(seq1_l, false);
        EXPECT_EQ(seq1_t, false);
        EXPECT_EQ(seq2_l, false);
        EXPECT_EQ(seq2_t, false);
    }

    { // custom
        end_gaps eg{second_seq_trailing{true}, front_end_first{std::true_type{}}, second_seq_leading{std::false_type{}}};

        constexpr bool seq1_l = decltype(eg)::is_static<0>();
        constexpr bool seq1_t = decltype(eg)::is_static<1>();
        constexpr bool seq2_l = decltype(eg)::is_static<2>();
        constexpr bool seq2_t = decltype(eg)::is_static<3>();

        EXPECT_EQ(seq1_l, true);
        EXPECT_EQ(seq1_t, false);
        EXPECT_EQ(seq2_l, true);
        EXPECT_EQ(seq2_t, false);
    }
}

TEST(end_gaps, static_access)
{
    end_gaps eg{second_seq_trailing{true}, front_end_first{std::true_type{}}, second_seq_leading{std::false_type{}}};

    constexpr bool seq1_l = decltype(eg)::get_static<0>();
    constexpr bool seq2_l = decltype(eg)::get_static<2>();

    EXPECT_EQ(seq1_l, true);
    EXPECT_EQ(eg[0], true);
    EXPECT_EQ(seq2_l, false);
    EXPECT_EQ(eg[2], false);
}

TEST(end_gaps, all_ends_free)
{
    using test = end_gaps<front_end_first<std::true_type>,
                          back_end_first<std::true_type>,
                          second_seq_leading<std::true_type>,
                          second_seq_trailing<std::true_type>>;
    EXPECT_EQ((std::is_same_v<std::remove_const_t<decltype(all_ends_free)>, test>), true);
}

TEST(end_gaps, none_ends_free)
{
    using test = end_gaps<front_end_first<std::false_type>,
                          back_end_first<std::false_type>,
                          second_seq_leading<std::false_type>,
                          second_seq_trailing<std::false_type>>;

    EXPECT_EQ((std::is_same_v<std::remove_const_t<decltype(none_ends_free)>, test>), true);
}

TEST(end_gaps, seq1_ends_free)
{
    using test = end_gaps<front_end_first<std::true_type>,
                          back_end_first<std::true_type>,
                          second_seq_leading<std::false_type>,
                          second_seq_trailing<std::false_type>>;
    EXPECT_EQ((std::is_same_v<std::remove_const_t<decltype(seq1_ends_free)>, test>), true);
}

TEST(end_gaps, seq2_ends_free)
{
    using test = end_gaps<front_end_first<std::false_type>,
                          back_end_first<std::false_type>,
                          second_seq_leading<std::true_type>,
                          second_seq_trailing<std::true_type>>;
    EXPECT_EQ((std::is_same_v<std::remove_const_t<decltype(seq2_ends_free)>, test>), true);
}

TEST(align_cfg_aligned_ends, is_aggregate)
{
    EXPECT_TRUE((std::is_aggregate_v<aligned_ends<end_gaps<>>>));
}

TEST(align_cfg_aligned_ends, id)
{
    align_cfg::aligned_ends cfg{all_ends_free};

    EXPECT_EQ(static_cast<uint8_t>(decltype(cfg)::id),
              static_cast<uint8_t>(detail::align_config_id::aligned_ends));
}

TEST(align_cfg_aligned_ends, value)
{
    align_cfg::aligned_ends cfg{seq1_ends_free};
    using type = decltype(cfg.value);

    using test = end_gaps<front_end_first<std::true_type>,
                          back_end_first<std::true_type>,
                          second_seq_leading<std::false_type>,
                          second_seq_trailing<std::false_type>>;

    EXPECT_EQ((std::is_same_v<type, test>), true);

    EXPECT_EQ(cfg.value[0], true);
    EXPECT_EQ(cfg.value[1], true);
    EXPECT_EQ(cfg.value[2], false);
    EXPECT_EQ(cfg.value[3], false);

    EXPECT_EQ(type::is_static<0>(), true);
    EXPECT_EQ(type::get_static<0>(), true);
    EXPECT_EQ(type::is_static<2>(), true);
    EXPECT_EQ(type::get_static<2>(), false);
}

TEST(align_cfg_aligned_ends, configuration)
{
    {
        align_cfg::aligned_ends elem{all_ends_free};
        configuration cfg{elem};

        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[0]), true);
        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[1]), true);
        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[2]), true);
        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[3]), true);
    }

    {
        configuration cfg{align_cfg::aligned_ends{all_ends_free}};

        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[0]), true);
        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[1]), true);
        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[2]), true);
        EXPECT_EQ((get<align_cfg::aligned_ends>(cfg).value[3]), true);
    }
}
