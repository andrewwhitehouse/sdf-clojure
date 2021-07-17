(ns sdf.chapter1)



(defn compose [f g]
    (let [the-composition (fn [& args] (f (apply g args)))]
        the-composition))
