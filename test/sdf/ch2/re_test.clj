(ns sdf.ch2.re-test
  (:require [clojure.test :refer :all]
            [sdf.ch2.re :refer :all]))

(deftest regular-expressions

  (testing "r:seq"
    (is (= "\\(\\(a\\).\\(c\\)\\)"
           (r:seq (r:quote "a") r:dot (r:quote "c"))))))
