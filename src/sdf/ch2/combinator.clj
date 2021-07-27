(ns sdf.ch2.combinator)

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

(defn discard-argument [i]
  (assert (nat-int? i))
  (fn [f]
    (let [m (inc (get-arity f))]
      (defn the-combination [& args]
        (assert (= (count args) m))
        (apply f (->> args (take i) (drop 1))))
      (assert (< i m))
      (restrict-arity the-combination m))))

(defn curry-argument [i]
  (fn [& args]
    (fn [f]
      (assert (= (count args) (dec (get-arity f))))
      (fn [x]
        (apply f
               (concat (take i args)
                       [x]
                       (drop i args)))))))

(defn make-permutation [permspec]
  (let [the-permuter (fn [lst]
                       (map (fn [p] (nth lst p)) permspec))]
    the-permuter))

(defn permute-arguments [& permspec]
  (let [permute (make-permutation permspec)]
    (fn [f]
      (let [the-combination (fn [& args] (apply f (permute args)))
            n (get-arity f)]
        (assert (= n (count permspec)))
        (restrict-arity the-combination n)))))
