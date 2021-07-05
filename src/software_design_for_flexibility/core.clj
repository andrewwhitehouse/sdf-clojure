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

;; ---------------------------------------------------


(defmacro get-metadata
  [func]
  `(meta (var ~func)))

(defn procedure-arity [f]
  (-> (get-metadata f) :arglists))

(defn procedure-arity-min [a]
    (->> a (map count) (reduce (fn [a b] (min a b)))))

(defn procedure-arity-max [a]
    (->> a (map count) (reduce (fn [a b] (max a b)))))



(defn get-arity [f]
    (or (-> f meta :arity)
        (let [a (procedure-arity f)]
            (assert (= (procedure-arity-min a)
                       (procedure-arity-max a)))
            (procedure-arity-min a))))

(defn restrict-arity [f nargs]
  (with-meta f {:arity nargs}))

#_(defn x [a b c] 1)
#_(get-arity (var x))


(defn spread-combine [h f g]
     (let [n (get-arity f)]
         (defn the-combination [& args]
             (h (apply f (take n args))
                (apply g (drop n args))))
         the-combination))

#_(defn f2 [x y] (list 'foo x y))
#_(defn f3 [u v w] (list 'bar u v w))

;; Applying the arity explicity seems to work best. If we do it through the
;; function metadata it gets a bit funky because we can only access it through the var
;; And what we really want is to tell spread-combine how many arguments
;; the first function should take

#_((spread-combine list
                 (restrict-arity (fn [x y] (list 'foo x y)) 2)
                 (fn [u v w] (list 'bar u v w)))
    'a 'b 'c 'd 'e)
