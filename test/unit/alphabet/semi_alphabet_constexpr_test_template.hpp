// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <gtest/gtest.h>

#include <bio/alphabet/concept.hpp>

template <typename t>
using semi_alphabet_constexpr = ::testing::Test;

TYPED_TEST_SUITE_P(semi_alphabet_constexpr);

TYPED_TEST_P(semi_alphabet_constexpr, concept_check)
{
    EXPECT_TRUE(bio::detail::constexpr_semialphabet<TypeParam>);
    EXPECT_TRUE(bio::detail::constexpr_semialphabet<TypeParam &>);

    EXPECT_TRUE(bio::detail::constexpr_semialphabet<TypeParam const>);
    EXPECT_TRUE(bio::detail::constexpr_semialphabet<TypeParam const &>);

    EXPECT_TRUE(bio::detail::writable_constexpr_semialphabet<TypeParam>);
    EXPECT_TRUE(bio::detail::writable_constexpr_semialphabet<TypeParam &>);

    EXPECT_FALSE(bio::detail::writable_constexpr_semialphabet<TypeParam const>);
    EXPECT_FALSE(bio::detail::writable_constexpr_semialphabet<TypeParam const &>);
}

TYPED_TEST_P(semi_alphabet_constexpr, default_value_constructor)
{
    [[maybe_unused]] constexpr TypeParam t0{};
}

TYPED_TEST_P(semi_alphabet_constexpr, assign_rank)
{
    constexpr size_t rank = 1 % bio::alphabet_size<TypeParam>;
    [[maybe_unused]] constexpr TypeParam t0{bio::assign_rank_to(rank, TypeParam{})};
}

TYPED_TEST_P(semi_alphabet_constexpr, to_rank)
{
    constexpr size_t rank = 1 % bio::alphabet_size<TypeParam>;
    constexpr TypeParam t0{bio::assign_rank_to(rank, TypeParam{})};
    constexpr bool b = (bio::to_rank(t0) == rank);
    EXPECT_TRUE(b);
}

TYPED_TEST_P(semi_alphabet_constexpr, copy_constructor)
{
    constexpr bio::alphabet_rank_t<TypeParam> rank = 1 % bio::alphabet_size<TypeParam>;
    constexpr TypeParam t1{bio::assign_rank_to(rank, TypeParam{})};

    constexpr TypeParam t2{t1};
    constexpr TypeParam t3(t1);
    EXPECT_EQ(t1, t2);
    EXPECT_EQ(t2, t3);
}

TYPED_TEST_P(semi_alphabet_constexpr, move_constructor)
{
    constexpr bio::alphabet_rank_t<TypeParam> rank = 1 % bio::alphabet_size<TypeParam>;
    constexpr TypeParam t0{bio::assign_rank_to(rank, TypeParam{})};
    constexpr TypeParam t1{t0};

    constexpr TypeParam t2{std::move(t1)};
    constexpr TypeParam t3(std::move(t2));
    EXPECT_EQ(t2, t0);
    EXPECT_EQ(t3, t0);
}

TYPED_TEST_P(semi_alphabet_constexpr, copy_assignment)
{
    constexpr size_t rank = 1 % bio::alphabet_size<TypeParam>;
    constexpr TypeParam t0{bio::assign_rank_to(rank, TypeParam{})};
    // constexpr context:
    constexpr TypeParam t3 = [&] () constexpr
    {
        TypeParam t1{bio::assign_rank_to(rank, TypeParam{})};
        TypeParam t2{};
        t2 = t1;

        return t2;
    }();
    EXPECT_EQ(t3, t0);
}

TYPED_TEST_P(semi_alphabet_constexpr, move_assignment)
{
    constexpr size_t rank = 1 % bio::alphabet_size<TypeParam>;
    constexpr TypeParam t0{bio::assign_rank_to(rank, TypeParam{})};
    // constexpr context:
    constexpr TypeParam t3 = [&] () constexpr
    {
        TypeParam t1{bio::assign_rank_to(rank, TypeParam{})};
        TypeParam t2{};
        t2 = std::move(t1);

        return t2;
    }();
    EXPECT_EQ(t3, t0);
}

TYPED_TEST_P(semi_alphabet_constexpr, comparison_operators)
{
    if constexpr (bio::alphabet_size<TypeParam> == 1)
    {
        constexpr TypeParam t0{};
        constexpr TypeParam t1{};
        constexpr bool b2 = (t0 <= t1);
        constexpr bool b3 = (t1 <= t1);
        constexpr bool b4 = (t1 == t1);
        constexpr bool b5 = (t1 >= t1);
        constexpr bool b6 = (t1 >= t0);

        EXPECT_TRUE(b2);
        EXPECT_TRUE(b3);
        EXPECT_TRUE(b4);
        EXPECT_TRUE(b5);
        EXPECT_TRUE(b6);
    }
    else
    {
        constexpr TypeParam t0{bio::assign_rank_to(0, TypeParam{})};
        constexpr TypeParam t1{bio::assign_rank_to(1, TypeParam{})};
        constexpr bool b1 = (t0 <  t1);
        constexpr bool b2 = (t0 <= t1);
        constexpr bool b3 = (t1 <= t1);
        constexpr bool b4 = (t1 == t1);
        constexpr bool b5 = (t1 >= t1);
        constexpr bool b6 = (t1 >= t0);
        constexpr bool b7 = (t1 >  t0);
        constexpr bool b8 = (t0 != t1);

        EXPECT_TRUE(b1);
        EXPECT_TRUE(b2);
        EXPECT_TRUE(b3);
        EXPECT_TRUE(b4);
        EXPECT_TRUE(b5);
        EXPECT_TRUE(b6);
        EXPECT_TRUE(b7);
        EXPECT_TRUE(b8);
    }
}

REGISTER_TYPED_TEST_SUITE_P(semi_alphabet_constexpr,
                            concept_check,
                            default_value_constructor,
                            assign_rank,
                            to_rank,
                            copy_constructor,
                            move_constructor,
                            copy_assignment,
                            move_assignment,
                            comparison_operators);
