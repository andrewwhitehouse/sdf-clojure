(ns sdf.chapter1-test
    (:require [clojure.test :refer :all]
              [sdf.chapter1 :refer :all]))

(deftest chapter1-tests
  (testing "compose"
     (let [compose-f (compose
                        (fn [x] (list 'foo x))
                        (fn [x] (list 'bar x)))]
          (is (= '(foo (bar z)) (compose-f 'z)))))

  (testing "my-iterate"
     (let [square (fn [x] (* x x))]
        (is (= 390625 (((my-iterate 3) square) 5)))))

  (testing "arity"
     (let [nargs 3
           f (restrict-arity (fn [& x] x) nargs)]
       (is (= nargs (get-arity f))))) )
