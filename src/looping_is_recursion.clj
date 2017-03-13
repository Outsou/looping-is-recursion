(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (<= n 0)
                   acc
                   (recur (* acc base) (dec n))))]
    (if (== exp 0)
      1
      (helper base (dec exp)))))

(defn last-element [a-seq]
  (let [helper (fn [b-seq]
                 (let [the-rest (rest b-seq)]
                   (if (empty? the-rest)
                     (first b-seq)
                     (recur the-rest))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2))
                     true
                   (or (empty? seq1) (empty? seq2))
                     false
                   (not= (first seq1) (first seq2))
                     false
                   :else
                     (recur (rest seq1) (rest seq2))
                   ))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [b-seq a-seq
         index 0]
    (cond
      (empty? b-seq)
        nil
      (pred (first b-seq))
        index
      :else
        (recur (rest b-seq) (inc index)))))

(defn avg [a-seq]
  (loop [b-seq a-seq
         sum 0
         element-count 0]
    (if (empty? b-seq)
      (if (== 0 element-count)
        0
        (/ sum element-count))
      (recur (rest b-seq)
             (+ sum (first b-seq))
             (inc element-count)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [b-seq a-seq
           odd-elems #{}]
      (if (empty? b-seq)
        odd-elems
        (recur (rest b-seq) (toggle odd-elems (first b-seq)))))))

(defn fast-fibo [n]
  (cond
    (== 0 n)
      0
    (or (== 1 n) (== 2 n))
      1
    :else
      (loop [f-1 1
             f-2 1
             count (- n 3)]
        (if (<= count 0)
          (+ f-1 f-2)
          (recur (+ f-1 f-2) f-1 (dec count))))))

(defn cut-at-repetition [a-seq]
  (loop [tail a-seq
         front []]
    (if (or (empty? tail)
            (some #{(first tail)} front))
      front
      (recur (rest tail) (conj front (first tail))))))

