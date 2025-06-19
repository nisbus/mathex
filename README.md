mathex
======
[![Build Status](https://api.travis-ci.org/nisbus/mathex.png?branch=master)](https://api.travis-ci.org/nisbus/mathex.png?branch=master) 

Extra math and statistics functions for Erlang.
The library provides calculations such as moving averages,
standard deviation, skewness, kurtosis and correlation.

The API can be used standalone from any Erlang application.
All functions operate on lists of numbers and return either a single
value or a list of values.

This library contains the following functions:

## Return a single value  
  
* average  
* sum  
* stdev_sample  
* stdev_population  
* skew  
* kurtosis  
* variance  
* covariance  

## Return a list of values  
  
* moving_average  
* correlation
* correlation_matrix

In addition to statistics, mathex can parse and evaluate simple algebraic
expressions.  See `mathex_calc:eval/1` for evaluating a string expression and
`mathex_calc:calculate/1` for applying an expression to a list of dated values.
  
Most of these are well documented elsewhere except maybe the  
correlation_matrix.  
The correlation matrix will take in a list of numeric lists  
and correlate each list in the collection with all the other lists  
of the collection.  
The result will look like this:  
  
```erlang
[{integer(),integer(),float()},
 {integer(),integer(),float()},
 {integer(),integer(),float()},
 {integer(),integer(),float()}]
```
  
Where the first int is the index of a list in the collection and the   
second integer the index of the list it's being correlated to.  
The float is the correlation of the two.  
It will give you all possible combinations and their correlation.  
  
  

## Running tests

EUnit tests are included and can be executed with:

```bash
rebar3 eunit
```

The tests cover the public API and a few edge cases.
  
  
## Documentation  
Check out the [gh-pages](http://nisbus.github.com/mathex)
