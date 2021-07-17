(ns sdf.chapter1-test
    (:require [clojure.test :refer :all]
              [sdf.chapter1 :refer :all]))

(deftest chapter1-tests
  (testing "compose"
     (let [compose-f (compose
                        (fn [x] (list 'foo x))
                        (fn [x] (list 'bar x)))]
          (is (= '(foo (bar z)) (compose-f 'z))))))
