;;; invariant_tests.clj -- Contracts programming library for Clojure

;; by Michael Fogus - http://fogus.me/fun/
;; Sept 16, 2010

;; Copyright (c) Michael Fogus, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns fogus.me.invariant-tests
  (:use [trammel.core :only [defconstrainedrecord]])
  (:use [clojure.test :only [deftest is]]))


(defconstrainedrecord Foo [a 1 b 2]
  [(every? number? [a b])]
  Object
  (toString [this] (str "record Foo has " a " and " b)))

(deftest test-constrained-record-with-vector-spec
  (is (= (:a (new-Foo)) 1))
  (is (= (:b (new-Foo)) 2))
  (is (= (:a (new-Foo :a 42)) 42))
  (is (= (:b (new-Foo :b 108)) 108))
  (is (= (:a (new-Foo :a 42 :b 108)) 42))
  (is (= (:b (new-Foo :a 42 :b 108)) 108))
  (is (= (:a (new-Foo :a 42 :b 108 :c 36)) 42))
  (is (= (:b (new-Foo :a 42 :b 108 :c 36)) 108))
  (is (= (:c (new-Foo :a 42 :b 108 :c 36)) 36))
  (is (thrown? Error (new-Foo :a :b)))
  (is (thrown? Error (new-Foo :a 42 :b nil))))

;; testing default clojure pre/post maps

(defconstrainedrecord Bar [a 1 b 2]
  {:pre [(every? number? [a b])]}
  Object
  (toString [this] (str "record Bar has " a " and " b)))

(deftest test-constrained-record-with-map-spec
  (is (= (:a (new-Bar)) 1))
  (is (= (:b (new-Bar)) 2))
  (is (= (:a (new-Bar :a 42)) 42))
  (is (= (:b (new-Bar :b 108)) 108))
  (is (= (:a (new-Bar :a 42 :b 108)) 42))
  (is (= (:b (new-Bar :a 42 :b 108)) 108))
  (is (= (:a (new-Bar :a 42 :b 108 :c 36)) 42))
  (is (= (:b (new-Bar :a 42 :b 108 :c 36)) 108))
  (is (= (:c (new-Bar :a 42 :b 108 :c 36)) 36))
  (is (thrown? Error (new-Bar :a :b)))
  (is (thrown? Error (new-Bar :a 42 :b nil))))
