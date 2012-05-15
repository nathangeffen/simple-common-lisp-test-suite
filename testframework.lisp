; Simple Common Lisp test package :cl-test-suite
; version 1.0 May 2012
;
; This is a simple generic test package.
;
; Programmer: Nathan Geffen (C) 2012. Licenced under GPL version
; 3.0. See below.
; 
; Email queries to nathangeffen at quackdown.info
;  
; Exported procedures:
;
;    add-test-case - adds a test case to a test suite list, where each
;                    test case is a an entry in the list
;
;    run-tests - takes a test-suite as a parameter, as well as two
;                    optional parameters to indicate output verbosity,
;                    and returns the number of failures and number of
;                    tests.
;
;    Six assert functions are supplied for convenience. Failure messages
;                    are printed if the assertions are not true.
;    asserteq - true if the first parameter is equal to the second
;                    parameter.
;
;    assertne - true if the first parameter is not equal to the second
;                    parameter.
;
;    assertge - true if the first parameter is >= the second parameter.
;
;    assertgt - true if the first parameter is > the second parameter.
;
;    assertle - true if the first parameter is <= the second parameter.
;
;    assertlt - true if the first parameter is less than the second
;                    parameter.
;
;    test-this-package - runs a series of tests to see that this test
;                    suite is working. The function should return 
;                    (0 . 8).
;
; Use run-tests to run a test suite.
; There are also several 
;
; Examples:
; 
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :cl-test-suite
  (:use :common-lisp)
  (:export :add-test-case
           :run-tests
           :asserteq
           :assertne
           :assertge
           :assertgt
           :assertle
           :assertlt
           :test-this-package))

(in-package :cl-test-suite)

(defmacro add-test-case (test-function 
                         parameter-list 
                         result-function 
                         expected-value
                         &optional test-suite) 
  "Macro to add a single test-case to a list of test-cases.

Parameters:
test-function - the function that is being tested
parameter-list - list of parameters to the testfunction
result-function - a function to apply to the result
expected-value - the value you expect (result-function 
                                        (test-function parameter-list)) to be
test-suite - the list containing the test cases (an optional field)

The macro returns the test-suite with the new test-case in the car position.
If test-suite is not passed, then it just returns the new test-case.

E.g: (add-test-case #'cadr ('(1 2 3 4 5 6)) #'identity 2 mytests)"
  (let* ((copy-test-suite test-suite)
         (new-test `(cons (list
                           :test-function ,test-function
                           :parameters (list ,@parameter-list)
                           :result-function ,result-function
                           :expected ,expected-value) ,copy-test-suite)))
    (if copy-test-suite
        `(setq ,copy-test-suite ,new-test)
        new-test)))

(defun single-test-case (test-function 
                         parameters 
                         result-function
                         expected)
    "This applies the function, test-function, with the parameters,
test-parameters. It then applies result-function to the result. If the
output of that equals test-expected then a list whose first parameter is
true is returned (as a list), else nil is returned along with the actual
result."                                
    (let ((result (funcall result-function
                           (apply 
                            test-function parameters))))
      (list (equal result expected) result expected)))

(defun run-tests (test-cases &optional (verbose t) (failure-messages t))
  "This runs a suite of tests defined in the test-cases parameter.

Parameters:
test-cases - list of cases to test (it's easiest to create these using
the add-test-case macro)
verbose - optional boolean that if true outputs success messages and
final summary messages (defaults to t)
failure-messages - optional boolean that if true outputs failure
messages (defaults to t)"
  (let ((num-test-cases 0)
        (failures 0))
    (dolist (test (map 'list
                       (lambda (test-case) 
                         (single-test-case 
                          (getf test-case :test-function)
                          (getf test-case :parameters)
                          (getf test-case :result-function)
                          (getf test-case :expected)))
                       test-cases))
      (when (not (car test))
        (setf failures (1+ failures))
        (if failure-messages
            (format t "Test ~D failed. Got ~a. Expected ~a.~%" 
                    num-test-cases (nth 1 test) (nth 2 test))))
      (setf num-test-cases (1+ num-test-cases)))
    (if verbose
        (format t "Test cases: ~D. Failures ~D.~%" num-test-cases failures))
    (cons failures num-test-cases)))

(defun asserteq (value expected)
  "Returns true if value = expected else outputs message and returns nil."
  (if (not (equal value expected))
      (progn
        (format t "ASSERTEQ Error: Got ~a. But expected ~a." value expected)
        nil)
      t))

(defun assertne (value expected)
  "Returns true if value <> expected else outputs message and returns nil."
    (if (equal value expected)
        (progn
          (format t "ASSERTNE Error: Values match but should not: ~a."
                value)
          nil)
        t))

(defun assertgt (value expected)
  "Returns true if value > expected else outputs message and returns nil."
  (if (not (> value expected))
      (progn
        (format t "ASSERTGT Error: Values match but should not: ~a."
                value)
        nil)
      t))

(defun assertge (value expected)
  "Returns true if value >= expected else outputs message and returns nil."
  (if (not (>= value expected))
      (progn
        (format t "ASSERTGT Error: Values match but should not: ~a."
                value)
        nil)
      t))

(defun assertlt (value expected)
  "Returns true if value < expected else outputs message and returns nil."
  (if (not (< value expected))
      (progn 
        (format t "ASSERTLT Error: Values match but should not: ~a."
                value)
        nil)
      t))

(defun assertle (value expected)
  "Returns true if value <= expected else outputs message and returns nil."
  (if (not (<= value expected))
      (progn
        (format t "ASSERTLE Error: Values match but should not: ~a."
                value)
        nil)
      t))


(defun test-this-package ()
  "Test cases for this package"
  (let ((test-suite nil)
        (failures 0)
        (num-tests 0))
    (format t "********************************************~%")
    (format t "Test package for Simple Lisp Test Framework.~%")
    (format t "--------------------------------------------~%")
    ; This should pass.
    (add-test-case #'car
                   ('(a b c d e f))
                   #'identity
                   'a
                   test-suite)

    ; This should pass too.
    (add-test-case #'apply
                   ('+ (list 2 3 4))
                   #'identity
                   '9
                   test-suite)

    ; This should fail. The actual is 9.
    (add-test-case #'apply
                   ('+ (list 2 3 4))
                   #'identity
                   '7
                   test-suite)

    ; This should pass.
    (add-test-case #'map
                   ('list '+ (list 2 3 4) (list 10 11 12))
                   #'car
                   '12
                   test-suite)

    ; Assert that there has been one failure.

    (if (asserteq (car (run-tests test-suite nil nil)) 1) 
        (format t "Passed run-tests on standard test cases.~%")
        (progn 
          (format t "Failed run-tests on standard cases.~%")
          (incf failures)))
    (incf num-tests)

    ; Assert that there have been four tests.
    (if (asserteq (cdr (run-tests (reverse test-suite) nil nil) ) 4)
        (format t "Passed run-tests in reverse test.~%")
        (progn 
          (format t "Failed run-tests in reverse test.~%")
          (incf failures)))
    (incf num-tests)

    ; Assert that has not been 5 tests
    (if (assertne (cdr (run-tests (reverse test-suite) nil nil) ) 5)
        (format t "Passed run-tests in reverse test.~%")
        (progn 
          (format t "Failed run-tests in reverse test.~%")
          (incf failures)))
    (incf num-tests)

    ; Assert that there have been greater than equal to four tests
    (if (assertge (cdr (run-tests (reverse test-suite) nil nil) ) 4)
        (format t "Passed assertge test.~%")
        (progn
          (format t "Failed assertge test.~%")
          (incf failures)))
    (incf num-tests)

    ; Assert that there have been less than equal to four tests
    (if (assertle (cdr (run-tests (reverse test-suite) nil nil) ) 4)
        (format t "Passed assertle test.~%")
        (progn 
          (format t "Failed assertle test.~%")
          (incf failures)))
    (incf num-tests)

    ; Assert that there have been less than five tests
    (if (assertgt (cdr (run-tests (reverse test-suite) nil nil) ) 3)
        (format t "Passed assertgt test.~%")
        (progn
          (format t "Failed assertgt test.")
          (incf failures)))
    (incf num-tests)

    ; Assert that there have been more than three  tests
    (if (assertlt (cdr (run-tests (reverse test-suite) nil nil) ) 5)
        (format t "Passed assertlt test.~%")
        (progn 
          (format t "Failed assertlt test.~%")
          (incf failures)))
    (incf num-tests)
        
    ; Test that run-tests works without optional test-suite parameter.
    (if (asserteq (car
                  (run-tests 
                   (add-test-case
                    #'apply
                    ('+ (list 2 3 4))
                    #'identity
                    '9)
                   nil nil))
        0)
        (format t "Passed run-tests called without test-suite parm.~%")
        (progn 
          (format t "Failed run-tests called without test-suite parm.~%")
          (incf failures)))
    (incf num-tests)
    (format t "Ran ~D tests. ~D failures.~%" num-tests failures)
    (format t "********************************************~%")
    (cons failures num-tests)))
        
        
