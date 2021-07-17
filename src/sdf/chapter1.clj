(ns sdf.chapter1)

(defn compose [f g]
    (let [the-composition (fn [& args] (f (apply g args)))]
        the-composition))

(defn my-iterate [n]
      (fn [f]
          (if (zero? n)
            identity
            (compose f ((my-iterate (dec n)) f)))))
