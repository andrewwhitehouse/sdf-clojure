(ns sdf.ch2.re)

(def r:dot ".")

(def r:bol "^")

(def r:eol "$")

(defn r:seq [& exprs]
  (str "\\(" (apply str exprs) "\\)"))

;; For list->string https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_7.html
(defn ^:private list->string [chs]
  (apply str chs))

(defn ^:private string->list [s]
  s)

(def ^:private chars-needing-quoting #{\. \[ \\ \^ \$ \*})

;; For append-map https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Mapping-of-Lists.html
(defn ^:private append-map [f & lsts]
  (apply list (map f lsts)))

(defn r:quote [s]
  (r:seq
   (list->string
    (map (fn [ch]
           (if (contains? chars-needing-quoting ch)
             (str \\ ch)
             (str ch)))
         (string->list s)))))

;; pair? https://cs.brown.edu/courses/csci1730/2008/Manual/guide/Pairs__Lists__and_Scheme_Syntax.html

(defn ^:private pair? [coll]
  (and (seq? coll) (not (empty? coll))))

(defn r:alt [& exprs]
  (if (pair? exprs)
    (apply r:seq
           (cons (first exprs)
                 (map (fn [expr] (str "\\|" expr))
                      (rest exprs))))
    (r:seq)))
