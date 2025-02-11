// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <list>
#include <type_traits>
#include <vector>

#include <gtest/gtest.h>

#include <bio/meta/type_traits/range.hpp>
#include <bio/meta/type_list/all.hpp>

TEST(type_list, basic)
{
    using t = bio::type_list<int, char, double>;

    EXPECT_TRUE((std::is_same_v<bio::list_traits::at<1, t>, char>));
}

// ----------------------------------------------------------------------------
// pack tests
// ----------------------------------------------------------------------------

TEST(pack_traits, size)
{
    EXPECT_EQ((bio::pack_traits::size<int, bool &, double const>),
               3u);
}

TEST(pack_traits, count)
{
    EXPECT_EQ((bio::pack_traits::count<int>),
               0u);
    EXPECT_EQ((bio::pack_traits::count<int, bool &, double const>),
               0u);
    EXPECT_EQ((bio::pack_traits::count<int, bool &, int, double const, int>),
               2u);
}

TEST(pack_traits, find)
{
    EXPECT_EQ((bio::pack_traits::find<int>),
               -1ll);
    EXPECT_EQ((bio::pack_traits::find<int, bool &, double const>),
               -1ll);
    EXPECT_EQ((bio::pack_traits::find<int, bool &, int, double const, int>),
               1u);
}

TEST(pack_traits, find_if)
{
    EXPECT_EQ((bio::pack_traits::find_if<std::is_integral>),
               -1ll);
    EXPECT_EQ((bio::pack_traits::find_if<std::is_integral, float, double const>),
               -1ll);
    EXPECT_EQ((bio::pack_traits::find_if<std::is_integral, float, int, double const, long>),
               1ll);
}

TEST(pack_traits, contains)
{
    EXPECT_EQ((bio::pack_traits::contains<int>),
               false);
    EXPECT_EQ((bio::pack_traits::contains<int, bool &, double const>),
               false);
    EXPECT_EQ((bio::pack_traits::contains<int, bool &, int, double const, int>),
               true);
}

TEST(pack_traits, at)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::at<2, int, bool &, double const, long, float>,
                                double const>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::at<-2, int, bool &, double const, long, float>,
                                long>));
}

TEST(pack_traits, front)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::front<int, bool &, double const, long, float>,
                                int>));
}

TEST(pack_traits, back)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::back<int, bool &, double const, long, float>,
                                float>));
}

TEST(pack_traits, drop_front)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::drop_front<int, bool &, double const, long, float>,
                                bio::type_list<bool &, double const, long, float>>));
}

TEST(pack_traits, transform)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::transform<std::ranges::range_value_t>,
                                bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::transform<std::ranges::range_value_t, std::vector<int>, std::list<bool>>,
                                bio::type_list<int, bool>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::transform<std::ranges::range_reference_t,
                                                               std::vector<int>,
                                                               std::list<bool>>,
                                bio::type_list<int &, bool &>>));
}

TEST(pack_traits, take)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::take<0, int, bool &, double const, long, float>,
                                bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::take<3, int, bool &, double const, long, float>,
                                bio::type_list<int, bool &, double const>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::take<5, int, bool &, double const, long, float>,
                                bio::type_list<int, bool &, double const, long, float>>));
}

TEST(pack_traits, drop)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::drop<0, int, bool &, double const, long, float>,
                                bio::type_list<int, bool &, double const, long, float>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::drop<3, int, bool &, double const, long, float>,
                                bio::type_list<long, float>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::drop<5, int, bool &, double const, long, float>,
                                bio::type_list<>>));
}

TEST(pack_traits, take_last)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::take_last<0, int, bool &, double const, long, float>,
                                bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::take_last<3, int, bool &, double const, long, float>,
                                bio::type_list<double const, long, float>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::take_last<5, int, bool &, double const, long, float>,
                                bio::type_list<int, bool &, double const, long, float>>));
}

TEST(pack_traits, drop_last)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::drop_last<0, int, bool &, double const, long, float>,
                                bio::type_list<int, bool &, double const, long, float>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::drop_last<3, int, bool &, double const, long, float>,
                                bio::type_list<int, bool &>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::drop_last<5, int, bool &, double const, long, float>,
                                bio::type_list<>>));
}

TEST(pack_traits, split_after)
{
    using split0 = bio::pack_traits::split_after<0, int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<typename split0::first_type,
                                bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<typename split0::second_type,
                                bio::type_list<int, bool &, double const, long, float>>));

    using split3 = bio::pack_traits::split_after<3, int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<typename split3::first_type,
                                bio::type_list<int, bool &, double const>>));
    EXPECT_TRUE((std::is_same_v<typename split3::second_type,
                                bio::type_list<long, float>>));


    using split5 = bio::pack_traits::split_after<5, int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<typename split5::first_type,
                                bio::type_list<int, bool &, double const, long, float>>));
    EXPECT_TRUE((std::is_same_v<typename split5::second_type,
                                bio::type_list<>>));
}

TEST(pack_traits, replace_at)
{
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::replace_at<double, 0, int, float, bool>,
                                bio::type_list<double, float, bool>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::replace_at<double, 1, int, float, bool>,
                                bio::type_list<int, double, bool>>));
    EXPECT_TRUE((std::is_same_v<bio::pack_traits::replace_at<double, 2, int, float, bool>,
                                bio::type_list<int, float, double>>));
}

// ----------------------------------------------------------------------------
// list tests
// ----------------------------------------------------------------------------

TEST(list_traits, size)
{
    EXPECT_EQ((bio::list_traits::size<bio::type_list<int, bool &, double const>>),
               3u);
}

TEST(list_traits, count)
{
    EXPECT_EQ((bio::list_traits::count<int, bio::type_list<>>),
               0u);
    EXPECT_EQ((bio::list_traits::count<int, bio::type_list<bool &, double const>>),
               0u);
    EXPECT_EQ((bio::list_traits::count<int, bio::type_list<bool &, int, double const, int>>),
               2u);
}

TEST(list_traits, find)
{
    EXPECT_EQ((bio::list_traits::find<int, bio::type_list<>>),
               -1ll);
    EXPECT_EQ((bio::list_traits::find<int, bio::type_list<bool &, double const>>),
               -1ll);
    EXPECT_EQ((bio::list_traits::find<int, bio::type_list<bool &, int, double const, int>>),
               1u);
}

TEST(list_traits, find_if)
{
    EXPECT_EQ((bio::list_traits::find_if<std::is_integral, bio::type_list<>>),
               -1ll);
    EXPECT_EQ((bio::list_traits::find_if<std::is_integral, bio::type_list<float, double const>>),
               -1ll);
    EXPECT_EQ((bio::list_traits::find_if<std::is_integral, bio::type_list<float, int, double const, long>>),
               1ll);
}

TEST(list_traits, contains)
{
    EXPECT_EQ((bio::list_traits::contains<int, bio::type_list<>>),
               false);
    EXPECT_EQ((bio::list_traits::contains<int, bio::type_list<bool &, double const>>),
               false);
    EXPECT_EQ((bio::list_traits::contains<int, bio::type_list<bool &, int, double const, int>>),
               true);
}

TEST(list_traits, at)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::at<2, test_types_list>, double const>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::at<-2, test_types_list>, long>));
}

TEST(list_traits, front)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::front<test_types_list>, int>));
}

TEST(list_traits, back)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::back<test_types_list>, float>));
}

TEST(list_traits, concat)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::concat<bio::type_list<int, bool &, double const>,
                                                            bio::type_list<long, float>>, test_types_list>));

    EXPECT_TRUE((std::is_same_v<bio::list_traits::concat<bio::type_list<int, bool &, double const>,
                                                            bio::type_list<long, float>,
                                                            bio::type_list<>,
                                                            bio::type_list<long &>>,
                                bio::type_list<int, bool &, double const, long, float, long &>>));
}

TEST(list_traits, drop_front)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::drop_front<test_types_list>,
                                bio::type_list<bool &, double const, long, float>>));
}

TEST(list_traits, take)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::take<0, test_types_list>, bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::take<3, test_types_list>,
                                bio::type_list<int, bool &, double const>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::take<5, test_types_list>, test_types_list>));
}

TEST(list_traits, drop)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::drop<0, test_types_list>, test_types_list>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::drop<3, test_types_list>, bio::type_list<long, float>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::drop<5, test_types_list>, bio::type_list<>>));
}

TEST(list_traits, take_last)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::take_last<0, test_types_list>, bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::take_last<3, test_types_list>,
                                bio::type_list<double const, long, float>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::take_last<5, test_types_list>, test_types_list>));
}

TEST(list_traits, drop_last)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    EXPECT_TRUE((std::is_same_v<bio::list_traits::drop_last<0, test_types_list>, test_types_list>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::drop_last<3, test_types_list>, bio::type_list<int, bool &>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::drop_last<5, test_types_list>, bio::type_list<>>));
}

TEST(list_traits, split_after)
{
    using test_types_list = bio::type_list<int, bool &, double const, long, float>;
    using split0 = bio::list_traits::split_after<0, test_types_list>;
    EXPECT_TRUE((std::is_same_v<typename split0::first_type, bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<typename split0::second_type, test_types_list>));

    using split3 = bio::list_traits::split_after<3, test_types_list>;
    EXPECT_TRUE((std::is_same_v<typename split3::first_type, bio::type_list<int, bool &, double const>>));
    EXPECT_TRUE((std::is_same_v<typename split3::second_type, bio::type_list<long, float>>));


    using split5 = bio::list_traits::split_after<5, test_types_list>;
    EXPECT_TRUE((std::is_same_v<typename split5::first_type, test_types_list>));
    EXPECT_TRUE((std::is_same_v<typename split5::second_type, bio::type_list<>>));
}

TEST(list_traits, transform)
{
    EXPECT_TRUE((std::is_same_v<bio::list_traits::transform<std::ranges::range_value_t,
                                                               bio::type_list<>>,
                                bio::type_list<>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::transform<std::ranges::range_value_t,
                                                               bio::type_list<std::vector<int>, std::list<bool>>>,
                                bio::type_list<int, bool>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::transform<std::ranges::range_reference_t,
                                                               bio::type_list<std::vector<int>, std::list<bool>>>,
                                bio::type_list<int &, bool &>>));
}

TEST(list_traits, replace_at)
{
    EXPECT_TRUE((std::is_same_v<bio::list_traits::replace_at<double, 0, bio::type_list<int, float, bool>>,
                                bio::type_list<double, float, bool>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::replace_at<double, 1, bio::type_list<int, float, bool>>,
                                bio::type_list<int, double, bool>>));
    EXPECT_TRUE((std::is_same_v<bio::list_traits::replace_at<double, 2, bio::type_list<int, float, bool>>,
                                bio::type_list<int, float, double>>));
}
