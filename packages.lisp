; Simple Common Lisp test package :cl-test-suite
; version 1.0 May 2012
;
; This is a simple generic test package.
;
; Programmer: Nathan Geffen (C) 2012. 
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

(in-package :cl-user)
(defpackage :cl-test-suite
  (:nicknames :test-suite)
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