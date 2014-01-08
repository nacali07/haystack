haystack
========

An R package of reference classes to manage the datasets, models, and 
processes of behavioral model development.


 These classes are designed to improve the efficiency of behavioral modeling 
 using R in financial institutions.  I wrote these classes to improve the
 organization, testability, and tracability of analytical processes.
 Specifically:

 1. Datasets, queries, and models are objects that can be passed by reference
    to functions and methods. This eliminates wasteful copying, while
    still allowing complex logic to exist in functions and methods.

 2. By putting as much analytical logic in the reference class, we reduce
    wasteful copying of code from one version of a model script to another.
    This also creates a framework for proper unit testing of functions and
    methods. 

 3. Each verion of a model exists and an object which can be easily copied
    into a new object and modified slightly. Saving the object will save
    all of the information needed to reproduce the result. 

 These classes utilize the recently developed reference classes.

 For an example of how I am using this, see ./examples/iris
