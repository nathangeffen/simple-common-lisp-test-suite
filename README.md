# Simple Common Lisp test package :cl-test-suite

## version 1.0 May 2012

Programmer: Nathan Geffen (C) 2012. Licenced under GPL version
3.0. See COPYING.
 
Email queries to nathangeffen at quackdown.info

This has been tested using SBCL.

### Exported procedures:

-  add-test-case - adds a test case to a test suite list, where each
   test case is a an entry in the list

- run-tests - takes a test-suite as a parameter, as well as two
  optional parameters to indicate output verbosity,
  and returns the number of failures and number of
  tests.
   
- Six assert functions are supplied for convenience. Failure messages
  are printed if the assertions are not true.

    - asserteq - true if the first parameter is equal to the second
      parameter.

    - assertne - true if the first parameter is not equal to the second
      parameter.

    - assertge - true if the first parameter is >= the second parameter.

    - assertgt - true if the first parameter is > the second parameter.

    - assertle - true if the first parameter is <= the second parameter.

    - assertlt - true if the first parameter is less than the second
      parameter.
- test-this-package - runs a series of tests to see that this test
  suite is working. The function should return 
  (0 . 8).

Example:

    (add-test-case #'map
                   ('list '+ (list 2 3 4) (list 10 11 12))
                   #'car
                   '12
                   test-suite)

    (run-tests test-suite)
    
For more examples, see the code for the *test-this-package* function in 
*testframework.lisp*.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
