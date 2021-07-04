(ns software-design-for-flexibility.core)



(defn compose [f g]
    (let [the-composition (fn [& args] (f (apply g args)))]
        the-composition))

#_((compose (fn [x] (list 'foo x))
          (fn [x] (list 'bar x)))
     'z)

;; ---------------------------------------------------

(defn my-iterate [n]
      (fn [f]
          (if (zero? n)
            identity
            (compose f ((my-iterate (dec n)) f)))))

(defn square [x] (* x x ))

#_(((my-iterate 3) square) 5)

;; ---------------------------------------------------

(defn parallel-combine [h f g]
    (defn the-combination [& args]
        (h (apply f args) (apply g args)))
    the-combination)

#_((parallel-combine list
                   (fn [x y z] (list 'foo x y z))
                   (fn [u v w] (list 'bar u v w)))
                    'a 'b 'c)
