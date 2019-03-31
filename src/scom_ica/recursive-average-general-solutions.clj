(use 'clojure.tools.trace)

(defn nested-average-tuple [tree]
  "Tail recursive solution that returns, in a tuple, both the total and count to calculate the average"
  (letfn [(get-tuple [lis]
            (loop [lst lis, total 0, count 0]
              (cond
                ;lst is number put the number (and count 1) in the tuple
                (number? lst) [lst 1]

                ;lst is empty therefore processed all items in lst put the final values of total and count in tuple
                (empty? lst) [total count]

                ;lst is non-empty sequence go back up to loop with the rest of the lst and current values
                ;(total and count) plus the values associated with the first item of lst
                (seq? lst)
                (let [x (get-tuple (first lst))] (recur (next lst) (+ total (x 0)) (+ count (x 1)))))))]

    (let [v (get-tuple tree)] (/ (v 0) (v 1)))))

(defn nested-average-1 [lst]
  "Simple one liner that flattens (puts items of nested list into the outer list) the list and reduces it to a sum then divides by the count"
  (let [flat (flatten lst)] (/ (reduce + flat) (count flat))))

(trace-vars nested-average-tuple)
(nested-average-tuple tree-1)