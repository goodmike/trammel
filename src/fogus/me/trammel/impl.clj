;;; impl.clj -- Contracts programming library for Clojure

;; by Michael Fogus - http://fogus.me/fun/
;; June 1, 2010

;; Copyright (c) Michael Fogus, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns fogus.me.trammel.impl)

(defn build-constraints-map [expectations]
  (apply merge
         (for [[[dir] & [cnstr]] (->> expectations
                                      (partition-by #{:requires :ensures})
                                      (partition 2))]
           {(case dir
                  :requires :pre
                  :ensures  :post)
            (vec cnstr)})))

(defn build-contract [[[sig] expectations :as c]]
  (list 
    (into '[f] sig)
    (build-constraints-map expectations)
    (list* 'f sig)))

(defn collect-bodies [forms]
  (for [body (->> (partition-by vector? forms)
                  (partition 2))]
    (build-contract body)))

(defn build-forms-map
  [forms]
  (for [[[e] & c] (map #(partition-by keyword? %) 
                       (if (vector? (first forms)) 
                         (list forms) 
                         forms))]
    {e (apply hash-map c)}))

(defmacro required 
  "Wrapper for `and` that has a better name: denotes :pre requirements"
  [& args]
  `(and ~@args))

(defn requirements 
  "Given a map of test sexp and string message, wraps these values, and the
   keyword :required, in a use of `and`.
   I'd sure rather be able to use the required macro instead"
  [mp] 
  (if (:msg mp) 
    (list 'and :required (:msg mp) (:test mp)) 
    (list 'and :required (:test mp))))


(defn prep-conditions [conditions]
  (letfn [(prep [[pair & partitioned-conditions]]
	  (if (and (string? (first pair)) (not (nil? (second pair))))
	    (cons {:msg (first pair) :test (second pair)}
		  (if (nil? (second partitioned-conditions))
		    '()
		    (prep (drop 1 partitioned-conditions))))
	    (cons {:msg nil :test (first pair)}
		  (if (nil? partitioned-conditions)
		    '()
		    (prep partitioned-conditions)))))]
    (prep (partition 2 1 '() conditions))))


