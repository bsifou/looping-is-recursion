(ns looping-is-recursion)

(defn power [base exp]
  (let  [helper (fn [acc exp]
                  (if (zero? exp)
                    acc
                    (recur (* base acc) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    :else (and (= (first seq1) (first seq2))
               (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         [fst & rest] a-seq]
    (cond (nil? fst) fst
          (pred fst) i
          :else (recur (inc i)
                       rest))))

;; (defn find-first-index [pred a-seq]
;;   (loop [i 0
;;          seq a-seq]
;;     (cond (empty? seq) nil
;;           (pred (first seq)) i
;;           :else (recur (inc i)
;;                        (rest seq)))))

(defn avg [a-seq]
  (loop [count 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum count)
      (recur (inc count)
             (+ (first seq) sum)
             (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [set #{}
         seq a-seq]
    (if (empty? seq)
      set
      (recur (toggle set (first seq))
             (rest seq)))))

;; (defn parity [a-seq]
;;   (loop [set (set a-seq)
;;          [fst & rest] a-seq]
;;     (if (nil? fst)
;;       set
;;       (recur (toggle set (first a-seq))
;;              rest))))

(defn fast-fibo [n]
  (loop [n n
         fibn 1
         fibn-1 0]
    (if (zero? n)
      fibn-1
      (recur (dec n)
             (+ fibn-1 fibn)
             fibn))))

(defn cut-at-repetition [a-seq]
  (loop [elems a-seq
         ret #{}]
    (if (or (empty? elems) (contains? ret (first elems)))
      (into [] ret)
      (recur (rest elems)
             (conj ret (first elems))))))


;; (defn cut-at-repetition [a-seq]
;;   (loop [elems a-seq
;;          ret #{}]
;;     (if (or (empty? elems) (contains? ret (first elems)))
;;       (reverse (into [] ret))
;;       (recur (rest elems)
;;              (conj ret (first elems))))))



(defn cut-at-repetition [a-seq]
  (loop [elems a-seq
         ret []]
    (if (or (empty? elems) (find-first-index (fn [elm] (= elm (first elems))) ret))
      ret
      (recur (rest elems)
             (conj ret (first elems))))))









