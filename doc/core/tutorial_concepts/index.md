# C++ Concepts {#core_concepts}

[TOC]

This tutorial introduces "C++ Concepts", a feature of C++20 (and available to some extent in older GCC versions).
You will learn the terminology used in the context of concepts and how to use BioC++'s concepts in your application.

\tutorial_head{Moderate, 60 min, \ref core_setup, }

This tutorial teaches the very basics of working with concepts. For more background and information on how to implement
your own concepts, we recommend:
  * A well-readable [paper](https://www.stroustrup.com/good_concepts.pdf) with motivation and historical background.
  * The (rather dense) [documentation on cppreference](https://en.cppreference.com/w/cpp/language/constraints).

# Constraints

## Motivation

One central design goal of BioC++ is to provide generic algorithms and data structures which can be used for different
types without reimplementing the same algorithms over and over again for particular types.
This has multiple benefits: improved maintainability due to an additional level of abstraction
and more importantly the ability to reuse the code with user provided types.
A familiar example for generic code is std::vector and the algorithms in the standard library.
They are *templates* which means that they can be *instantiated* with other types.
Most often the type cannot be arbitrary, because the template expects a particular interface from the type.

A BioC++ example is the local alignment algorithm.
It computes the best local match between two sequences over a finite alphabet.
The algorithm is generic in so far that it allows any alphabet that offers the minimal interface which
is used inside the algorithm (e.g. objects of the alphabet type must be equality comparable).
Before C++20, this could not be checked easily and using the interface with non-conforming types would result in
very hard to read compiler errors and consequently frustration of the user.
In the following part of the tutorial you will learn how to *constrain* such template arguments of generic functions
and data structures and how this can have a huge impact on your code.

Here's a shorter example:

```cpp
template <typename t>
t add(t const v1, t const v2)
{
    return v1 + v2;
}

int main()
{
    return add(1, 3); // instantiates add<int>()
}
```

The template parameter `t` is said to be *unconstrained*, in theory it can be instantiated with any type.
But of course it won't actually compile for all types, because the function template **implicitly requires** that
types provide a `+` operator.
If a type is used that does not have a `+` operator, this implicitness causes the compiler to fail at the place where
such operator is used – and not at the place the template is instantiated.
This leads to very complex error messages for deeply nested code.

*Constraints* are a way of making requirements of template arguments **explicit**.
Constraints can be formulated ad-hoc, but this tutorial only covers *concepts*.
The interested reader can check the [documentation](https://en.cppreference.com/w/cpp/language/constraints) to learn
about ad-hoc definitions.
Concepts are a set of constraints with a given name.
Let's assume there is a concept called `Addable` that requires the existence of a `+` operator (as previously mentioned
the syntax for defining concepts is not covered here).
The following snippet demonstrates how we can constrain our function template, i.e. make the template immediately
reject any types that don't satisfy the requirement:

```cpp
template <Addable t>
t add(t const v1, t const v2)
{
    return v1 + v2;
}

int main()
{
    return add(1, 3); // instantiates add<int>()
}
```

The only difference is that we have replaced `typename` with `Addable`.
If you plug in a type that does not model `Addable`, you will get a message stating exactly that and not a cryptic
template backtrace.

The standard library provides a set of [predefined concepts](https://en.cppreference.com/w/cpp/concepts).
For our example above, the std::integral concept could have been used.

## Syntax variants

Depending on the complexity of your constraint statements, three different syntaxes are available to enforce
constraints; all of the following are equivalent.

(1) The "verbose syntax", especially useful when enforcing multiple constraints:

```cpp
template <typename t1, typename t2>
    requires std::integral<t1> && std::integral<t2> // && MyOtherConcept<t1>
auto add(t1 const v1, t2 const v2)
{
    return v1 + v2;
}
```

(2) The "intermediate syntax":
```cpp
template <std::integral t1, std::integral t2>                       // one constraint per type
auto add(t1 const v1, t2 const v2)
{
    return v1 + v2;
}
```

(3) The "terse syntax":
```cpp
auto add(std::integral auto const v1, std::integral auto const v2)  // one constraint per type
{
    return v1 + v2;
}
```

\attention The terse syntax in this form is not yet available in GCC7, GCC8 and GCC9.

Different constraints can be applied to different template parameters and a single template parameter can be constrained
by multiple concepts.
Syntaxes can also be combined:
```cpp
template <std::integral t1, std::integral t2>
    // requires MyOtherConcept<t1>
auto add(t1 const v1, t2 const v2)
{
    return v1 + v2;
}
```

# Terminology

  * Template arguments can be ***constrained***.
  * A named set of constraints is a ***concept***.
  * A type that satisfies all requirements of a concept is said to ***model*** said concept.
  * A *concept* that is composed of another concept and additional constraints is said to ***refine*** said concept(s).

Some people confuse concepts with *interfaces*.
Both can be used as an abstraction of concrete types, but interfaces have to be inherited from. → the abstraction
is explicit in the definition of the type.
Concepts on the other hand "describe properties from the outside". → types don't need to be related and don't need
to "know about the concept" to model it.

Furthermore, the polymorphism possible with concepts (see below) is faster, because it is resolved at compile-time while
interface inheritance is resolved at run-time.

# Overloading and specialisation

In generic programming, "function overloading" and "template specialisation" play an important role.
They allow providing generic interfaces and (gradually) more specialised implementations for specific types or groups
of types.

## Function (template) overloading

When a function is overloaded and multiple overloads are valid for a given/deduced template argument, the
*most-refined* overload is chosen:

\include overloading1.cpp

But as soon as we introduce another overload, the compiler will pick the "best" match:

\include overloading2.cpp

\assignment{Assignment 1: Static polymorphism with alphabets I}
Write a small program, similar to the one above with the following "skeleton":
```cpp
// which includes?

using bio::operator""_dna5;
using bio::operator""_aa27;

// Add one or more `void print` function template(s) here //

int main()
{
    auto d = 'A'_dna5;
    auto a = 'L'_aa27;
    auto g = bio::gap{};

    print(d);
    print(a);
    print(g);
}
```

The `print` function (template) should print for every object `v` passed to it the result of `to_char(v)` and it should
be constrained to only accepts types that model bio::alphabet.
Try calling `print` with a different type, e.g. `int` to make sure that it does.
\endassignment
\solution
\include overloading_solution1.cpp
\endsolution

\assignment{Assignment 2: Static polymorphism with alphabets II}
Adapt your previous solution to handle nucleotides differently from the rest. For nucleotides, it should print both the value and its complement.
\endassignment
\solution
\include overloading_solution2.cpp
\endsolution

## Partial template specialisation

Similar to function template overloading it is possible to use concepts for partially specialising class and variable
templates.

\include specialisation.cpp

This is a typical example of a "type transformation trait".
It maps one type to another type; in this case it returns a type that is able to represent the square root of the\n"input type".
This can be used in generic algorithms to hold data in different types depending on the type of the input –
in this case we could avoid half of the space consumption for unsigned integral types VS signed integral types.

\note The std::same_as used above is a concept with two template parameters.
It requires that both parameters are the same. The `static_assert` checks conditions at compile-time; it can be
used to verify whether a type or a combination of types model a concept. In the above case we can use the combination
to check the "return type" of the transformation trait.

# Concepts in BioC++ and this documentation

BioC++ uses concepts extensively, for specialisation/overloading, but also to prevent misuse of templates and to clearly
specify all public interfaces.
We prefer the intermediate syntax and additionally use the verbose expressions if necessary.
Unfortunately, doxygen, the system used to generate this documentation, does not handle C++ concepts very well, yet.
In some parts of the documentation concepts are called "interfaces", please don't let this confuse you.
And the "verbose syntax" introduced above is not visible at all in the automatically generated documentation.
That's why it's important to read the detailed documentation section where all requirements are documented.

Have a look at the documentation of bio::argument_parser::add_positional_option().
It has two template parameters, one seems unconstrained (`typename` in the signature) and one is constrained
(`validator` in the signature).
But in fact both are constrained as the detailed documentation reveals.

Now, follow the link to bio::validator. We will check in the next section whether you understand the
documentation for the concept.

