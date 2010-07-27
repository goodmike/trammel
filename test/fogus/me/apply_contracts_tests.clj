;;; defcontract_tests.clj -- Contracts programming library for Clojure

;; by Michael Fogus - http://fogus.me/fun/
;; May 26, 2010

;; Copyright (c) Michael Fogus, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns fogus.me.apply-contracts-tests
  (:use [fogus.me.trammel :only [apply-contracts]])
  (:use [clojure.test :only [deftest is]]))

(defn sqr [n] (* n n))

(apply-contracts
 [sqr [n] :requires number? (not= 0 n)
          :ensures pos? number?])

(deftest apply-contracts-test
  (is (= 25 (sqr 5)))
  (is (= 25 (sqr -5)))
  (is (thrown? Error (sqr 0))))