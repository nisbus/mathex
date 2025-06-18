mathex
======
[![Build Status](https://api.travis-ci.org/nisbus/mathex.png?branch=master)](https://api.travis-ci.org/nisbus/mathex.png?branch=master) 

Extra math functions for Erlang

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
  
  
# To Do  
Add a sort function to return the correlation_matrix sorted desc/asc.  
Add more functions.  
Add unit tests.
Add more documentation.

## MCP server and client

This project exposes the statistics functions over a minimal line-based TCP
protocol.  The easiest way to run the server is via Docker:

```bash
docker build -t mathex .
docker run -p 4040:4040 mathex
```

Once running, you can query the server without any Erlang knowledge by using
the provided Python script:

```bash
python3 tools/mcp_client.py sum 1 2 3
```

The client connects to `localhost:4040` by default and prints the result of the
requested function.


## Documentation
Check out the [gh-pages](http://nisbus.github.com/mathex)
