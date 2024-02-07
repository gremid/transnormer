(ns transnormer.collate
  "Alignment algorithms based on Myer's diff."
  (:import com.github.difflib.DiffUtils
           [com.github.difflib.patch AbstractDelta Chunk]))

;; ## Pair-wise alignment
;;
;; Myer's diff aligns 2 comparands. Its implementation in `difflib`, being
;; designed to be used for diffing and patching files, returns `Delta` objects
;; as the representation of found differences. Each `Delta` aligns two `Chunk`
;; objects – a chunk being a subsequence of a comparand, and tags the alignment
;; with an edit operation (insert, delete, change).
;;
;; The following functions take the `difflib` output and turn it back into the
;; original token-wise alignment as represented in the edit graph that is at the
;; core of Myer's diff.

(defn chunk->range
  "Turns difflib's `Chunk` objects into offset range vectors."
  [^Chunk chunk]
  (let [offset (.getPosition chunk)
        length (.size chunk)]
    [offset (+ offset length)]))

(defn deltas->ranges
  "Turns difflib's `Delta` objects into offset range vectors."
  ([a b deltas]
   (deltas->ranges a b 0 0 deltas))
  ([a b ai bi [^AbstractDelta delta & deltas]]
   (if-not delta
     (when (or (< ai (count a)) (< bi (count b)))
       [[(subvec a ai) (subvec b bi)]])
     (let [^Chunk a-chunk (.getSource delta)
           ^Chunk b-chunk (.getTarget delta)
           [as ae] (chunk->range a-chunk)
           [bs be] (chunk->range b-chunk)
           aligned (when (or (< ai as) (< bi bs))
                     [[(subvec a ai as) (subvec b bi bs)]])
           diff (if (or (= as ae) (= bs be))
                  ;; insert or delete
                  [[(subvec a as ae) (subvec b bs be)]]
                  ;; change
                  [[(subvec a as ae) []] [[] (subvec b bs be)]])]
       (lazy-cat aligned diff (deltas->ranges a b ae be deltas))))))

(defn ranges->alignment
  "Turns offset range vectors into an alignment table."
  [ranges]
  (for [[a b] ranges
        :let [gaps (repeat (max (count a) (count b)) 0)
              a (or (not-empty a) gaps)
              b (or (not-empty b) gaps)
              alignments (mapv vector a b)]
        alignment alignments]
    alignment))

(defn align*
  "Aligns 2 colls, returning an alignment table."
  ([a b]
   (->> (.getDeltas (DiffUtils/diff a b))
        (deltas->ranges a b)
        (ranges->alignment)
        (vec))))

(comment
  (align* [1 2 3] [1 3 4 5]))

;; ## Progressive alignment
;;
;; To align more than 2 comparands, we extend the basic alignment algorithm as
;; provided above by merging comparands step-by-step into a common base.

(defn base-token
  [base]
  (some #(and (pos? %) %) base))

(defn merge-into-base
  ([base alignment]
   (let [base-gap (vec (repeat (-> base first count) 0))]
     (merge-into-base base alignment base-gap)))
  ([base alignment base-gap]
   (let [[base-head & base-rest]             base
         [[a b :as align-head] & align-rest] alignment]
     (when align-head
       (cons (if (pos? a) (conj base-head b) (conj base-gap b))
             (lazy-seq (merge-into-base
                        (if (pos? a) base-rest base)
                        align-rest
                        base-gap)))))))

(defn align-with-base
  ([base b]
   (let [a         (vec (map base-token base))
         alignment (align* a b)]
     (vec (merge-into-base base alignment)))))

(defn progressively-align
  ([[c1 & comparands]]
   (let [base (vec (map vector c1))]
     (reduce (partial align-with-base) base comparands))))

(comment
  (progressively-align [[1 2 3] [1 3 4 5] [2 3 4] [2 3 5]]))

;; ## Clustering of comparands based on neighbor joining.

(defn num-pairs
  [n]
  (for [i (range n) j (range (inc i) n)]
    [i j]))

(defn coll-pairs
  [coll]
  (for [[i j] (num-pairs (count coll))]
    [(nth coll i) (nth coll j)]))

(defn key-pairs
  [m]
  (-> m keys vec coll-pairs))

(defn triples->matrix
  [triples]
  (reduce
   (fn [m [i j v]] (-> (assoc-in m [i j] v) (assoc-in [j i] v)))
   {}
   triples))

(defn distance-matrix
  [measure-dist comparands]
  (->>
   (num-pairs (count comparands))
   (pmap (fn [[i j]] [i j (measure-dist (nth comparands i) (nth comparands j))]))
   (triples->matrix)))

(defn avg-distances
  [dist-matrix]
  (into {} (for [[k vs] dist-matrix :let [dists (vals vs)]]
             [k (/ (apply + dists) (count dists))])))

(defn closest-neighbors
  [dist-matrix]
  (let [avgs (avg-distances dist-matrix)]
    (reduce
     (fn [[_ _ dist-1 :as triple-1] [_ _ dist-2 :as triple-2]]
       (if (<= dist-1 dist-2) triple-1 triple-2))
     (for [[i j] (key-pairs dist-matrix)]
       [i j (- (get-in dist-matrix [i j]) (get avgs i) (get avgs j))]))))

(defn measure-neighbors-dist
  [dist-matrix k nb-1 nb-2]
  (let [nb-1->k (get-in dist-matrix [nb-1 k])
        nb-2->k (get-in dist-matrix [nb-2 k])
        nb-1->nb-2 (get-in dist-matrix [nb-1 nb-2])]
    (/ (- (+ nb-1->k nb-2->k) nb-1->nb-2) 2)))

(defn join-neighbors*
  [dist-matrix]
  (let [[nb-1 nb-2 _] (closest-neighbors dist-matrix)
        nbs #{nb-1 nb-2}]
    (list
     [nb-1 nb-2]
     (triples->matrix
      (concat
       (for [[i j] (key-pairs dist-matrix) :when (and (not (nbs i)) (not (nbs j)))]
         [i j (get-in dist-matrix [i j])])
       (for [k (keys dist-matrix) :when (not (nbs k))]
         [k [nb-1 nb-2] (measure-neighbors-dist dist-matrix k nb-1 nb-2)]))))))

(defn order-by-dist
  [measure-dist comparands]
  (loop [result [] dist-matrix (distance-matrix measure-dist comparands)]
    (if (= (count dist-matrix) 2)
      (concat result (->> dist-matrix keys (filter number?) sort))
      (let [[next-result next-dist-matrix] (join-neighbors* dist-matrix)]
        (recur (concat result (filter number? next-result)) next-dist-matrix)))))

;; ## Spencer-Howe Progressive Alignment

(defn gap-distance
  "Distance as the number of gaps."
  ([a b]
   (count (filter #(some zero? %) (align* a b)))))

(defn align
  ([comparands]
   (align gap-distance comparands))
  ([measure-dist comparands]
   (let [comparands  (vec comparands)
         n           (count comparands)
         comparands' (vec (order-by-dist measure-dist comparands))]
     (vec
      (for [row (progressively-align (map comparands comparands'))]
        (vec (map (comp row (zipmap comparands' (range n))) (range n))))))))

(comment
  (align [[1 2 3] [1 3 4 5] [2 3 4] [1 3 4 5] [3 5 1]]))

;; ## Token Mappings

(defn create-token-mapping
  ([]
   (create-token-mapping {}))
  ([m]
   {:m (atom m) :n (atom (count m))}))

(def ^:dynamic *token-mapping*
  (create-token-mapping))

(defn token->n
  [token]
  (get
   (swap! (*token-mapping* :m)
          update token (fn [v] (or v (swap! (*token-mapping* :n) inc))))
   token))

(defn n->token
  [nums tokens]
  (loop [result                               []
         [n & rest-nums]                      nums
         [token & rest-tokens :as all-tokens] tokens]
    (cond
      (nil? n)  result
      (zero? n) (recur (conj result nil) rest-nums all-tokens)
      :else     (recur (conj result token) rest-nums rest-tokens))))

(defn comparands->matrix
  [comparands]
  (vec (for [c comparands] (vec (map token->n c)))))

(defn matrix->comparands
  [alignment comparands]
  (vec
   (for [[a c] (partition 2 (interleave alignment comparands))]
     (n->token a c))))

(defn transpose
  [m]
  (apply mapv vector m))

(defn align-tokens
  [comparands]
  (binding [*token-mapping* (create-token-mapping)]
    (-> comparands comparands->matrix align
        transpose (matrix->comparands comparands) transpose)))

(comment
  (align-tokens [[:a :b :c] [:a :c :d :e] [:b :c :d] [:a :c :d :e] [:c :e :a]]))
