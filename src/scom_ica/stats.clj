(def stats-symbols {
                    '("min" :min) :min,
                    '("max" :max) :max,
                    '("count" :count) :count,
                    '("average" :average) :average })

(defn get-keys [] (keys stats-symbols))

(defn get-stat-key [symbol]
  (first (filter #(some #{symbol} %) (keys stats-symbols))))

(defn stats [tree & stats]
  (let [flattened (flatten tree), calculated-stats (assoc {} :min (min flattened) :max (max flattened) :count (count flattened) :average (nested-average-1 tree))]
    (select-keys calculated-stats (map (stats-symbols (get-stat-key symbol)) stats))))

(defn flat-compare [coll comparator]
  (loop [lst coll, minimum (first coll)]
    (cond
      (empty? lst) minimum
        (comparator (first lst) minimum) (recur (rest lst) (first lst))

      :else (recur (rest lst) minimum))))

(defn nested-compare [tree comparator]
  ;if tree is a sequence get the reference of the first item in the tree otherwise return the number (as the reference)
  (let [reference-first (if (seq? tree) (nested-compare (first tree) comparator) tree)]

    ;already evaluated the reference (from the first item) so assign lst the value of the rest of tree
    (loop [lst (if (seq? tree) (rest tree) tree), reference reference-first]
      (cond
        ;lst is a number just compare with the reference and return result of comparison
        (number? lst) (if (comparator lst reference) lst reference)

        ;lst is empty return the final reference for lst
        (empty? lst) reference

        ;lst is a sequence get the reference (min, max, etc) of the first, recur
        ;with rest of lst and result of comparing reference of first with current reference
        (seq? lst)
          (let [x (nested-compare (first lst) comparator)] (recur (rest lst) (if (comparator x reference) x reference)))))))

(nested-compare tree <)