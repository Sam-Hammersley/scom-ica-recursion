(defn nested-count-head [tree]
  (cond
    ;tree is one number therefore one item, return 1 (base case)
    (number? tree) 1

    ;tree is empty therefore no items, return 0 (base case)
    (empty? tree) 0

    ;tree is a non-empty sequence so add the count of the rest of the tree to the count of the first item (recursive step)
    (seq? tree) (+ (nested-count-head (next tree)) (nested-count-head (first tree)))))

(defn nested-sum-tree-head [tree]
  (cond
    ;tree is a number so return the value of that number (base case)
    (number? tree) tree

    ;tree is empty thus total is 0 (base case)
    (empty? tree) 0

    ;tree is a non-empty sequence so add sum of the rest of the tree to sum of the first of the tree (recursive step)
    (seq? tree) (+ (nested-sum-tree-head (next tree)) (nested-sum-tree-head (first tree)))))

(defn nested-average-head [tree] (let [total (nested-sum-tree-head tree), count (nested-count-head tree)] (/ total count)))

(nested-average-head tree)


