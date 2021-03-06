; provide_contracts_tests.clj

; by Michael Fogus - http://fogus.me/fun/
; May 26, 2010

; Copyright (c) Michael Fogus, 2010. All rights reserved.  The use
; and distribution terms for this software are covered by the Eclipse
; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING the root of this
; distribution.  By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.  You must not
; remove this notice, or any other, from this software.

(ns fogus.me.provide-contracts-tests
  (:use [trammel.core :only [defcontract]])
  (:require [trammel.provide :as provide])
  (:use [clojure.test :only [deftest is]]))

(defn sqr [n]
  (* n n))

(provide/contracts
  [sqr "The constraining of sqr" [n] [number? (not= 0 n) => pos? number?]])

(deftest apply-contracts-test
  (is (= 25 (sqr 5)))
  (is (= 25 (sqr -5)))
  (is (thrown? Error (sqr 0))))

(defn sqr2 [n]
  (* n n))

(defcontract sqr-contract
  "Defines the constraints on squaring."
  [n] [number? (not= 0 n) => pos? number?])

(provide/contracts
  [sqr2 "Apply the contract for squaring" sqr-contract])

(deftest apply-existing-contract-test
  (is (= 25 (sqr2 5)))
  (is (= 25 (sqr2 -5)))
  (is (thrown? Error (sqr2 0))))
