(use 'clojure.tools.trace)

(defn nested-count-tail [tree]
  (loop [lst tree, count 0]
    (cond
      ;lst is one number therefore one item, return 1 (base case)
      (number? lst) (inc count)

      ;tree is empty therefore the lst has been counted in previous call, return count (base case)
      (empty? lst) count

      ;lst is a non-empty sequence so go back up to loop with the rest of lst
      ;and the count of the first item plus the current count (recursive step)
      (seq? lst)
      (recur (next lst) (+ (nested-count-tail (first lst)) count)))))

(defn nested-sum-tree-tail [tree]
  (loop [lst tree, total 0]
    (cond
      ;lst is a number so add to total (base case)
      (number? lst) (+ lst total)

      ;lst is an empty sequence so return the total (base case)
      (empty? lst) total

      ;lst is a non-empty sequence so recursive call with the rest of tree and the current total
      ;plus the return value of a recursive call with the first of item of tree (recursive step)
      (seq? lst)
      (recur (next lst) (+ (nested-sum-tree-tail (first lst)) total)))))

(defn nested-average-tail [tree] (let [total (nested-sum-tree-tail tree), count (nested-count-tail tree)] (/ total count)))

(trace-vars nested-sum-tree-tail)
(nested-average-tail tree-1)

(defn nested-average [tree]
   (loop [head (first tree), tail (rest tree), total 0, count 0]
     (cond
       ;head is num continue with rest of the list, new total and inc count (recursive step)
       (number? head) (recur (first tail) (rest tail) (+ head total) (inc count))

       ;head is empty sequence return mean (base case)
       (empty? head) (/ total count)

       ;head is a non-empty sequence recur
       (seq? head) (recur (first head) (concat (rest head) tail) total count))))

