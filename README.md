<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/JDOsborne1/UltraSpoofR.svg?branch=master)](https://travis-ci.org/JDOsborne1/UltraSpoofR)
[![Codecov test
coverage](https://codecov.io/gh/JDOsborne1/UltraSpoofR/branch/master/graph/badge.svg)](https://codecov.io/gh/JDOsborne1/UltraSpoofR?branch=master)
<!-- badges: end -->

UltraSpoofR
===========

The goal of UltraSpoofR is to modify the typical workflow such that the
raw genuine data can be disconnected from the bulk of the workflow. It
will do this by providing a new data object, which has an authenticated
link to the actual data, along with a data dictionary to describe that
data and a reasonable spoof of that data.

TODOs
-----

Add in regex case insensitivity to type argument of the anticiapator
dict –TODO

New Spoofing system
-------------------

The new spoofing mechanism planned will ideally remove the breaking
cases where column wise spoofing was introducing illegal combinations of
values, or multiples of some combinations where those combinations were
unique in the data.

It will summarise at all the nominal variables, and for each combination
it will calculate a random value for all value columns. This will form
the first phase of the spoof.

If there is sufficient columns in the first phase, it will be sampled
down to 100 by default

if there is not, the mechanism will run again, binding the additional
rows in all cases where the combination count is greater than 1.

This process will repeat until either 100+ rows have been spoofed or
there is no more combinations with sufficient count to be included

Functions needed:

-   \~\~Qualifier function which can identify the ordinal columns in a
    way which would be accepted by vars in summarise at/ alternatively a
    boolean function which can be accepted by summarise if. \~\~

-   \~\~Random generator function which can act on a grouped column.
    \~\~

-   wrapper function which applies the above process and exploits the
    above function to create a spoofed dataset

Overse Mechanics
----------------

The Overse project is an attempt to add tooling to an R analysis or
investigative workflow. The tooling has been concieved as a mechanism to
remove the occurance of times when through time. policy changes or
employment changes, the meaning and functionality of an analysis becomes
lost. It will do this by filling a process with documentation, and
making it easy to act with moderation in terms of access to the full
database.

The underlying data may move, and there’s no guarantee even the original
author will be able to smoothly apply the work done in that project to
another one without any working examples.

You may want to build in tests to functions, but aren’t
comfortable/allowed to embed sensitive data in your functions.

An author may leave, and all the background work put into figuring out
the underlying methodology behind a variable may become lost. And even
if documented, it may go missing.

Workflow
--------

1.  Understand your data sources and objective
2.  Explore your data sources, creating anticipator origin functions
    around the datasets that you want to work with.
3.  Document those datasets with Ultra’s metadata structure, and any
    other relevant specific comments as attributes.
4.  Once finished understanding and exploring your objectives and data
    you can spoof the data. This will provide you a representative
    imitation of 100 rows of your data, but which don’t actually
    correspond to any real entries.
5.  When approaching a task it is worthwhile setting out your goals, in
    the form of text before the section, and ideally, test functions to
    ensure the desired behavior is happening.
6.  Using the spoofed datasets, build your analytical step.
7.  Construct your analysis, repeating 5 & 6 as necessary.
8.  As you find your analytical steps repeating, you can transform them
    into functions, which may already have tests available.
9.  By storing the ultra\_df object with your analysis, you embed the
    metadata and workable example data with your analysis, aiding future
    investigation. By only including a properly constructed anticipator
    as the origin function, all access to the actual data can be as
    restrictive and transient as necessary.

### Special caveat for testing

If you have packaged up analysis, and you have formal tests for your
functions, it is rarely acceptable to include sensitive data in the
package for testing, or if you do you often restrict the avenues where
you can share your projects. Resorting to openly available data to test
your packages runs the risk of the functions failing on the exact
structure of your possibly confidential data. Since the spoofer data has
fundamentally the same structure as the origin data, then it can be
included without issue.
