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
            'a 'b 'c 'd 'e))))

  (testing "spread-combine v2"
    (is (= '((foo a b) (bar c d e))
           ((spread-combine-2 identity
                              (restrict-arity (fn [x y] (list 'foo x y)) 2)
                              (restrict-arity (fn [u v w] (list 'bar u v w)) 3))
            'a 'b 'c 'd 'e))))

  (testing "curry-argument"
    (let [f (restrict-arity (fn [x y z w] (list 'foo x y z w)) 4)]
      (is (= '(foo a b d c)
             ((((curry-argument 2) 'a 'b 'c) f) 'd)))))

  (testing "permute-arguments"
    (is (= '(foo b c a d)
           (((permute-arguments 1 2 0 3)
             (restrict-arity (fn [x y z w] (list 'foo x y z w)) 4))
            'a 'b 'c 'd)))))
