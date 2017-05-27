(ns looping-is-recursion)

(defn power-helper [base exp acc]
  (if (zero? exp)
    acc
    (recur base (dec exp) (* base acc))))

(defn power [x y]
  (loop [base x
         exp y
         acc 1]
    (if (zero? exp)
      acc
      (recur base (dec exp) (* base acc)))))


(defn last-element-helper [a-seq acc]
  (if (empty? a-seq)
    acc
    (recur (rest a-seq) (first a-seq))))

(defn last-element [a-seq]
  (last-element-helper a-seq nil))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))


(defn find-first-index [pred a-seq]
  (loop [acc 0
         func pred
         seq a-seq]
    (cond
      (empty? seq) nil
      (func (first seq)) acc
      :else (recur (inc acc) func (rest seq)))))


(defn avg [a-seq]
  (loop [the-seq a-seq
         n 0
         acc 0]
   (if (empty? the-seq)
      (/ acc n)
      (recur (rest the-seq) (inc n) (+ acc (first the-seq))))))


(defn toggle [a-map elem]
  (cond
    (empty? a-map) (conj a-map elem)
    (contains? a-map elem) (disj a-map elem)
    :else (conj a-map elem)))

(defn parity [a-seq]
  (loop [src-seq a-seq
         trgt-seq #{}]
    (if (empty? src-seq)
      trgt-seq
      (recur (rest src-seq) (toggle trgt-seq (first src-seq))))))

(toggle #{} :a)
(toggle #{:a} :a)
(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}


(defn fast-fibo [n]
  (loop [target n
         current 0
         n 0
         n-one 1]
        (cond
          (= current target) n
          :else (recur target (inc current) n-one (+ n n-one)))))

(defn cut-at-repetition [a-seq]
  [":("])
