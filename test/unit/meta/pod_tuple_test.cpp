// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <type_traits>

#include <gtest/gtest.h>

#include <bio/meta/pod_tuple.hpp>

// default/zero construction
TEST(pod_tuple_ctr, ctr)
{
    [[maybe_unused]] bio::pod_tuple<int, long, float> t1;
}

// aggregate initialization
TEST(pod_tuple_aggr, aggr)
{
    [[maybe_unused]] bio::pod_tuple<int, long, float> t1{4, 7l, 3.0f};
}

// zero initialization
TEST(pod_tuple_zro, zro)
{
    bio::pod_tuple<int, long, float> t1{0, 0, 0};
    bio::pod_tuple<int, long, float> t2{};

    EXPECT_EQ(t1, t2);
}

// copy construction
TEST(pod_tuple_cp_ctr, cp_ctr)
{
    bio::pod_tuple<int, long, float> t1{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t2{t1};
    bio::pod_tuple<int, long, float> t3(t1);
    EXPECT_EQ(t1, t2);
    EXPECT_EQ(t2, t3);
}

// move construction
TEST(pod_tuple_mv_ctr, mv_ctr)
{
    bio::pod_tuple<int, long, float> t0{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t1{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t2{std::move(t1)};
    EXPECT_EQ(t2, t0);
    bio::pod_tuple<int, long, float> t3(std::move(t2));
    EXPECT_EQ(t3, t0);
}

// copy assignment
TEST(pod_tuple_cp_assgn, cp_assgn)
{
    bio::pod_tuple<int, long, float> t1{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t2;
    bio::pod_tuple<int, long, float> t3;

    t2 = t1;
    t3 = t1;
    EXPECT_EQ(t1, t2);
    EXPECT_EQ(t2, t3);
}

// move assignment
TEST(pod_tuple_mv_assgn, mv_assgn)
{
    bio::pod_tuple<int, long, float> t0{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t1{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t2;
    bio::pod_tuple<int, long, float> t3;
    t2 = std::move(t1);
    EXPECT_EQ(t2, t0);
    t3 = std::move(t2);
    EXPECT_EQ(t3, t0);
}

// swap
TEST(pod_tuple_swap, swap)
{
    bio::pod_tuple<int, long, float> t0{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t1{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t2{};
    bio::pod_tuple<int, long, float> t3{};

    std::swap(t1, t2);
    EXPECT_EQ(t2, t0);
    EXPECT_EQ(t1, t3);
}

// get<1>
TEST(pod_tuple_get_i, get_i)
{
    bio::pod_tuple<int, long, float> t0{4, 7l, 3.0f};

    static_assert(std::is_same_v<decltype(bio::get<0>(t0)), int &>);
    static_assert(std::is_same_v<decltype(bio::get<1>(t0)), long &>);
    static_assert(std::is_same_v<decltype(bio::get<2>(t0)), float &>);
    EXPECT_EQ(bio::get<0>(t0), 4);
    EXPECT_EQ(bio::get<1>(t0), 7l);
    EXPECT_EQ(bio::get<2>(t0), 3.0f);
}

// std::get<1>
TEST(pod_tuple, stdget_i)
{
    bio::pod_tuple<int, long, float> t0{4, 7l, 3.0f};

    static_assert(std::is_same_v<decltype(std::get<0>(t0)), int &>);
    static_assert(std::is_same_v<decltype(std::get<1>(t0)), long &>);
    static_assert(std::is_same_v<decltype(std::get<2>(t0)), float &>);
    EXPECT_EQ(std::get<0>(t0), 4);
    EXPECT_EQ(std::get<1>(t0), 7l);
    EXPECT_EQ(std::get<2>(t0), 3.0f);
}

// structured bindings
TEST(pod_tuple_struct_binding, struct_binding)
{
    bio::pod_tuple<int, long, float> t0{4, 7l, 3.0f};
    auto [ i, l, f ] = t0;

    EXPECT_EQ(i, 4);
    EXPECT_EQ(l, 7l);
    EXPECT_EQ(f, 3.0f);
}

// get<type>
TEST(pod_tuple_get_type, get_type)
{
    using pt = bio::pod_tuple<int, long, float>;
    using ptc = pt const;
    pt t0{4, 7l, 3.0f};
    ptc t1{4, 7l, 3.0f};

    static_assert(std::is_same_v<decltype(bio::get<int>(t0)), int &>);
    static_assert(std::is_same_v<decltype(bio::get<long>(t0)), long &>);
    static_assert(std::is_same_v<decltype(bio::get<float>(t0)), float &>);

    static_assert(std::is_same_v<decltype(bio::get<int>(t1)), int const &>);
    static_assert(std::is_same_v<decltype(bio::get<long>(t1)), long const &>);
    static_assert(std::is_same_v<decltype(bio::get<float>(t1)), float const &>);

    static_assert(std::is_same_v<decltype(bio::get<int>(pt{4, 7l, 3.0f})), int &&>);
    static_assert(std::is_same_v<decltype(bio::get<long>(pt{4, 7l, 3.0f})), long &&>);
    static_assert(std::is_same_v<decltype(bio::get<float>(pt{4, 7l, 3.0f})), float &&>);

    static_assert(std::is_same_v<decltype(bio::get<int>(ptc{4, 7l, 3.0f})), int const &&>);
    static_assert(std::is_same_v<decltype(bio::get<long>(ptc{4, 7l, 3.0f})), long const &&>);
    static_assert(std::is_same_v<decltype(bio::get<float>(ptc{4, 7l, 3.0f})), float const &&>);

    EXPECT_EQ(bio::get<int>(t0), 4);
    EXPECT_EQ(bio::get<long>(t0), 7l);
    EXPECT_EQ(bio::get<float>(t0), 3.0f);

    EXPECT_EQ(bio::get<int>(t1), 4);
    EXPECT_EQ(bio::get<long>(t1), 7l);
    EXPECT_EQ(bio::get<float>(t1), 3.0f);

    EXPECT_EQ(bio::get<int>(pt{4, 7l, 3.0f}), 4);
    EXPECT_EQ(bio::get<long>(pt{4, 7l, 3.0f}), 7l);
    EXPECT_EQ(bio::get<float>(pt{4, 7l, 3.0f}), 3.0f);

    EXPECT_EQ(bio::get<int>(ptc{4, 7l, 3.0f}), 4);
    EXPECT_EQ(bio::get<long>(ptc{4, 7l, 3.0f}), 7l);
    EXPECT_EQ(bio::get<float>(ptc{4, 7l, 3.0f}), 3.0f);
}

// std::get<type>
TEST(pod_tuple_get_type, stdget_type)
{
    using pt = bio::pod_tuple<int, long, float>;
    using ptc = pt const;
    pt t0{4, 7l, 3.0f};
    ptc t1{4, 7l, 3.0f};

    static_assert(std::is_same_v<decltype(std::get<int>(t0)), int &>);
    static_assert(std::is_same_v<decltype(std::get<long>(t0)), long &>);
    static_assert(std::is_same_v<decltype(std::get<float>(t0)), float &>);

    static_assert(std::is_same_v<decltype(std::get<int>(t1)), int const &>);
    static_assert(std::is_same_v<decltype(std::get<long>(t1)), long const &>);
    static_assert(std::is_same_v<decltype(std::get<float>(t1)), float const &>);

    static_assert(std::is_same_v<decltype(std::get<int>(pt{4, 7l, 3.0f})), int &&>);
    static_assert(std::is_same_v<decltype(std::get<long>(pt{4, 7l, 3.0f})), long &&>);
    static_assert(std::is_same_v<decltype(std::get<float>(pt{4, 7l, 3.0f})), float &&>);

    static_assert(std::is_same_v<decltype(std::get<int>(ptc{4, 7l, 3.0f})), int const &&>);
    static_assert(std::is_same_v<decltype(std::get<long>(ptc{4, 7l, 3.0f})), long const &&>);
    static_assert(std::is_same_v<decltype(std::get<float>(ptc{4, 7l, 3.0f})), float const &&>);

    EXPECT_EQ(std::get<int>(t0), 4);
    EXPECT_EQ(std::get<long>(t0), 7l);
    EXPECT_EQ(std::get<float>(t0), 3.0f);

    EXPECT_EQ(std::get<int>(t1), 4);
    EXPECT_EQ(std::get<long>(t1), 7l);
    EXPECT_EQ(std::get<float>(t1), 3.0f);

    EXPECT_EQ(std::get<int>(pt{4, 7l, 3.0f}), 4);
    EXPECT_EQ(std::get<long>(pt{4, 7l, 3.0f}), 7l);
    EXPECT_EQ(std::get<float>(pt{4, 7l, 3.0f}), 3.0f);

    EXPECT_EQ(std::get<int>(ptc{4, 7l, 3.0f}), 4);
    EXPECT_EQ(std::get<long>(ptc{4, 7l, 3.0f}), 7l);
    EXPECT_EQ(std::get<float>(ptc{4, 7l, 3.0f}), 3.0f);
}

// std::tuple_element
TEST(pod_tuple_tuple_element, tuple_element)
{
    using pt = bio::pod_tuple<int, long, float>;

    static_assert(std::is_same_v<std::tuple_element_t<0, pt>, int>);
    static_assert(std::is_same_v<std::tuple_element_t<1, pt>, long>);
    static_assert(std::is_same_v<std::tuple_element_t<2, pt>, float>);
    static_assert(std::tuple_size_v<pt> == 3);
}

// type deduction
TEST(pod_tuple_type_deduce, type_deduce)
{
    bio::pod_tuple t0{4, 7l, 3.0f};
    using pt = decltype(t0);

    static_assert(std::is_same_v<std::tuple_element_t<0, pt>, int>);
    static_assert(std::is_same_v<std::tuple_element_t<1, pt>, long>);
    static_assert(std::is_same_v<std::tuple_element_t<2, pt>, float>);
}

// comparison operators
TEST(pod_tuple_cmp, cmp)
{
    bio::pod_tuple<int, long, float> t0{4, 6l, 4.0f};
    bio::pod_tuple<int, long, float> t1{4, 7l, 3.0f};
    bio::pod_tuple<int, long, float> t2{4, 7l, 4.0f};

    EXPECT_LT(t0, t1);
    EXPECT_LE(t0, t1);
    EXPECT_LE(t1, t1);
    EXPECT_EQ(t1, t1);
    EXPECT_GE(t1, t1);
    EXPECT_GE(t2, t1);
    EXPECT_GT(t2, t1);
}
