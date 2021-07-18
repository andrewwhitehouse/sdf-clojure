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
      (is (= nargs (get-arity f)))))

  (testing "spread-combine"
    (is (= '((foo a b) (bar c d e))
           ((spread-combine list
                            (restrict-arity (fn [x y] (list 'foo x y)) 2)
                            (fn [u v w] (list 'bar u v w)))
            'a 'b 'c 'd 'e)))))
