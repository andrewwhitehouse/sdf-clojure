(ns sdf.chapter1)

(defn compose [f g]
  (let [the-composition (fn [& args] (f (apply g args)))]
    the-composition))

(defn my-iterate [n]
  (fn [f]
    (if (zero? n)
      identity
      (compose f ((my-iterate (dec n)) f)))))

(defn get-arity [f]
  (-> f meta :arity))

(defn restrict-arity [f nargs]
  (with-meta f {:arity nargs}))

(defn spread-combine [h f g]
  (let [n (get-arity f)]
    (defn the-combination [& args]
      (h (apply f (take n args))
         (apply g (drop n args))))
    the-combination))

(defn spread-apply [f g]
  (let [n (get-arity f)
        m (get-arity g)
        t (+ n m)
        the-combination (fn [& args]
                          (assert (= (count args) t))
                          (list (apply f (take n args))
                                (apply g (drop n args))))]
    (restrict-arity the-combination t)))

(defn spread-combine-2 [h f g]
  (comp h (spread-apply f g)))
