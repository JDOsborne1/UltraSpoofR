<!-- README.md is generated from README.Rmd. Please edit that file -->
UltraSpoofR
===========

The goal of UltraSpoofR is to modify the typical workflow such that the
raw genuine data can be disconnected from the bulk of the workflow. It
will do this by providing a new data object, which has an authenticated
link to the actual data, along with a data dictionary to describe that
data and a reasonable spoof of that data.

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

TODOs
-----

Add in regex case insensitivity to type argument of the anticiapator
dict –TODO

~~Add in Metadata dictionary interaction and functionality: likely
involving big update to megametadata~~ –TODO

~~introduce data spoofing elements to mimic data types with random
data~~ –TODO

\~\~constrain the spoof generation to the features of the underlying
data, min max, type combos etc \~\~–TODO

Add further tests to the output of the spoofing functionality –TODO

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